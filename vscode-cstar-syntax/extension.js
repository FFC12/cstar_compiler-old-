"use strict";

const cp = require("child_process");
const path = require("path");
const vscode = require("vscode");

let client = null;

function activate(context) {
  const output = vscode.window.createOutputChannel("C* Static Analyzer");
  context.subscriptions.push(output);

  const start = () => {
    stop();
    if (!vscode.workspace.getConfiguration("cstar").get("enableAnalyzer", true)) {
      return;
    }
    client = new CStarAnalyzerClient(context, output);
    client.start();
    context.subscriptions.push(client);
  };

  context.subscriptions.push(vscode.commands.registerCommand("cstar.restartAnalyzer", start));
  context.subscriptions.push(vscode.workspace.onDidChangeConfiguration(event => {
    if (event.affectsConfiguration("cstar.enableAnalyzer") ||
        event.affectsConfiguration("cstar.analyzer.serverPath") ||
        event.affectsConfiguration("cstar.analyzer.nodePath") ||
        event.affectsConfiguration("cstar.analyzer.trace")) {
      start();
    }
  }));

  start();
}

function deactivate() {
  stop();
}

function stop() {
  if (client) {
    client.dispose();
    client = null;
  }
}

class CStarAnalyzerClient {
  constructor(context, output) {
    this.context = context;
    this.output = output;
    this.proc = null;
    this.buffer = Buffer.alloc(0);
    this.nextId = 1;
    this.pending = new Map();
    this.versions = new Map();
    this.diagnostics = vscode.languages.createDiagnosticCollection("cstar-static");
    this.trace = vscode.workspace.getConfiguration("cstar").get("analyzer.trace", false);
    this.changeTimers = new Map();
  }

  start() {
    const config = vscode.workspace.getConfiguration("cstar");
    const nodePath = config.get("analyzer.nodePath", "node");
    const configuredServer = config.get("analyzer.serverPath", "");
    const serverPath = configuredServer || path.join(this.context.extensionPath, "server", "server.js");

    this.output.appendLine(`Starting C* analyzer: ${nodePath} ${serverPath}`);
    this.proc = cp.spawn(nodePath, [serverPath], {
      cwd: this.context.extensionPath,
      stdio: ["pipe", "pipe", "pipe"],
      windowsHide: true
    });

    this.proc.stdout.on("data", chunk => this.onData(chunk));
    this.proc.stderr.on("data", chunk => this.output.append(chunk.toString()));
    this.proc.on("error", error => {
      this.output.appendLine(`Analyzer failed to start: ${error.message}`);
      vscode.window.showWarningMessage(`C* analyzer failed to start: ${error.message}`);
    });
    this.proc.on("exit", (code, signal) => {
      this.output.appendLine(`C* analyzer exited (${code ?? "null"}${signal ? `, ${signal}` : ""}).`);
    });

    this.request("initialize", {
      processId: process.pid,
      rootUri: vscode.workspace.workspaceFolders?.[0]?.uri.toString() || null,
      capabilities: {}
    }).then(() => {
      this.notify("initialized", {});
      this.registerDocumentHooks();
      for (const document of vscode.workspace.textDocuments) {
        if (document.languageId === "cstar") this.didOpen(document);
      }
    }).catch(error => {
      this.output.appendLine(`Analyzer initialize failed: ${error.message}`);
    });

    this.completionProvider = vscode.languages.registerCompletionItemProvider(
      { language: "cstar", scheme: "*" },
      {
        provideCompletionItems: async (document, position) => {
          try {
            const result = await this.request("textDocument/completion", {
              textDocument: { uri: document.uri.toString() },
              position: toLspPosition(position)
            });
            return new vscode.CompletionList((result.items || []).map(fromLspCompletion), result.isIncomplete);
          } catch (error) {
            this.output.appendLine(`Completion failed: ${error.message}`);
            return undefined;
          }
        }
      },
      ".",
      ":",
      "<",
      " "
    );
  }

  registerDocumentHooks() {
    this.openSub = vscode.workspace.onDidOpenTextDocument(document => {
      if (document.languageId === "cstar") this.didOpen(document);
    });
    this.changeSub = vscode.workspace.onDidChangeTextDocument(event => {
      if (event.document.languageId === "cstar") this.didChange(event.document);
    });
    this.closeSub = vscode.workspace.onDidCloseTextDocument(document => {
      if (document.languageId === "cstar") this.didClose(document);
    });
  }

  didOpen(document) {
    this.versions.set(document.uri.toString(), document.version);
    this.notify("textDocument/didOpen", {
      textDocument: {
        uri: document.uri.toString(),
        languageId: "cstar",
        version: document.version,
        text: document.getText()
      }
    });
  }

  didChange(document) {
    const uri = document.uri.toString();
    clearTimeout(this.changeTimers.get(uri));
    this.changeTimers.set(uri, setTimeout(() => {
      this.versions.set(uri, document.version);
      this.notify("textDocument/didChange", {
        textDocument: { uri, version: document.version },
        contentChanges: [{ text: document.getText() }]
      });
    }, 150));
  }

  didClose(document) {
    const uri = document.uri.toString();
    this.versions.delete(uri);
    clearTimeout(this.changeTimers.get(uri));
    this.changeTimers.delete(uri);
    this.notify("textDocument/didClose", {
      textDocument: { uri }
    });
    this.diagnostics.delete(document.uri);
  }

  onData(chunk) {
    this.buffer = Buffer.concat([this.buffer, chunk]);
    while (true) {
      const headerEnd = this.buffer.indexOf("\r\n\r\n");
      if (headerEnd === -1) return;
      const header = this.buffer.slice(0, headerEnd).toString("ascii");
      const lengthMatch = /Content-Length:\s*(\d+)/i.exec(header);
      if (!lengthMatch) {
        this.buffer = this.buffer.slice(headerEnd + 4);
        continue;
      }
      const length = Number(lengthMatch[1]);
      const start = headerEnd + 4;
      const end = start + length;
      if (this.buffer.length < end) return;
      const raw = this.buffer.slice(start, end).toString("utf8");
      this.buffer = this.buffer.slice(end);
      const message = JSON.parse(raw);
      this.handleMessage(message);
    }
  }

  handleMessage(message) {
    if (this.trace) this.output.appendLine(`<= ${JSON.stringify(message)}`);
    if (message.method === "textDocument/publishDiagnostics") {
      const uri = vscode.Uri.parse(message.params.uri);
      const diagnostics = (message.params.diagnostics || []).map(fromLspDiagnostic);
      this.diagnostics.set(uri, diagnostics);
      return;
    }
    if (message.method === "window/logMessage") {
      this.output.appendLine(message.params.message);
      return;
    }
    if (Object.prototype.hasOwnProperty.call(message, "id")) {
      const pending = this.pending.get(message.id);
      if (pending) {
        this.pending.delete(message.id);
        if (message.error) pending.reject(new Error(message.error.message || "Analyzer request failed"));
        else pending.resolve(message.result);
      }
    }
  }

  request(method, params) {
    const id = this.nextId++;
    const promise = new Promise((resolve, reject) => {
      this.pending.set(id, { resolve, reject });
      setTimeout(() => {
        if (this.pending.has(id)) {
          this.pending.delete(id);
          reject(new Error(`${method} timed out`));
        }
      }, 5000);
    });
    this.send({ jsonrpc: "2.0", id, method, params });
    return promise;
  }

  notify(method, params) {
    this.send({ jsonrpc: "2.0", method, params });
  }

  send(payload) {
    if (!this.proc || !this.proc.stdin.writable) return;
    if (this.trace) this.output.appendLine(`=> ${JSON.stringify(payload)}`);
    const json = JSON.stringify(payload);
    this.proc.stdin.write(`Content-Length: ${Buffer.byteLength(json, "utf8")}\r\n\r\n${json}`);
  }

  dispose() {
    this.openSub?.dispose();
    this.changeSub?.dispose();
    this.closeSub?.dispose();
    this.completionProvider?.dispose();
    this.diagnostics.dispose();
    for (const timer of this.changeTimers.values()) clearTimeout(timer);
    this.changeTimers.clear();
    if (this.proc) {
      try {
        this.notify("shutdown", {});
        this.notify("exit", {});
      } catch (_) {
        // best effort
      }
      setTimeout(() => {
        if (this.proc && !this.proc.killed) this.proc.kill();
      }, 250);
    }
  }
}

function toLspPosition(position) {
  return { line: position.line, character: position.character };
}

function fromLspRange(range) {
  return new vscode.Range(
    new vscode.Position(range.start.line, range.start.character),
    new vscode.Position(range.end.line, range.end.character)
  );
}

function fromLspDiagnostic(diagnostic) {
  const item = new vscode.Diagnostic(
    fromLspRange(diagnostic.range),
    diagnostic.message,
    fromLspSeverity(diagnostic.severity)
  );
  item.code = diagnostic.code;
  item.source = diagnostic.source || "cstar-static";
  return item;
}

function fromLspSeverity(severity) {
  if (severity === 1) return vscode.DiagnosticSeverity.Error;
  if (severity === 2) return vscode.DiagnosticSeverity.Warning;
  if (severity === 3) return vscode.DiagnosticSeverity.Information;
  return vscode.DiagnosticSeverity.Hint;
}

function fromLspCompletion(item) {
  const completion = new vscode.CompletionItem(item.label, fromLspCompletionKind(item.kind));
  completion.detail = item.detail;
  completion.documentation = item.documentation;
  if (item.insertText) {
    completion.insertText = item.insertTextFormat === 2
      ? new vscode.SnippetString(item.insertText)
      : item.insertText;
  }
  return completion;
}

function fromLspCompletionKind(kind) {
  const map = {
    2: vscode.CompletionItemKind.Method,
    3: vscode.CompletionItemKind.Function,
    4: vscode.CompletionItemKind.Constructor,
    5: vscode.CompletionItemKind.Field,
    6: vscode.CompletionItemKind.Variable,
    7: vscode.CompletionItemKind.Class,
    8: vscode.CompletionItemKind.Interface,
    9: vscode.CompletionItemKind.Module,
    10: vscode.CompletionItemKind.Property,
    14: vscode.CompletionItemKind.Keyword,
    15: vscode.CompletionItemKind.Snippet,
    25: vscode.CompletionItemKind.TypeParameter
  };
  return map[kind] || vscode.CompletionItemKind.Text;
}

module.exports = { activate, deactivate };
