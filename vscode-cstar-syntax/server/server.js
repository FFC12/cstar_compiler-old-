"use strict";

const { analyzeText, computeCompletions } = require("./analyzer");

const documents = new Map();
let input = Buffer.alloc(0);
let isShutdown = false;

process.stdin.on("data", chunk => {
  input = Buffer.concat([input, chunk]);
  processMessages();
});

process.stdin.resume();

function processMessages() {
  while (true) {
    const headerEnd = input.indexOf("\r\n\r\n");
    if (headerEnd === -1) return;

    const header = input.slice(0, headerEnd).toString("ascii");
    const lengthMatch = /Content-Length:\s*(\d+)/i.exec(header);
    if (!lengthMatch) {
      input = input.slice(headerEnd + 4);
      continue;
    }

    const contentLength = Number(lengthMatch[1]);
    const messageStart = headerEnd + 4;
    const messageEnd = messageStart + contentLength;
    if (input.length < messageEnd) return;

    const raw = input.slice(messageStart, messageEnd).toString("utf8");
    input = input.slice(messageEnd);

    try {
      handleMessage(JSON.parse(raw));
    } catch (error) {
      log(`Failed to parse message: ${error.message}`);
    }
  }
}

function handleMessage(message) {
  if (message.method === "initialize") {
    respond(message.id, {
      capabilities: {
        textDocumentSync: {
          openClose: true,
          change: 1
        },
        completionProvider: {
          triggerCharacters: [".", ":", "<", " "],
          resolveProvider: false
        }
      },
      serverInfo: {
        name: "cstar-static-analyzer",
        version: "0.1.0"
      }
    });
    return;
  }

  if (message.method === "shutdown") {
    isShutdown = true;
    respond(message.id, null);
    return;
  }

  if (message.method === "exit") {
    process.exit(isShutdown ? 0 : 1);
  }

  if (message.method === "textDocument/didOpen") {
    const doc = message.params.textDocument;
    documents.set(doc.uri, doc.text);
    publishDiagnostics(doc.uri, doc.text);
    return;
  }

  if (message.method === "textDocument/didChange") {
    const uri = message.params.textDocument.uri;
    const change = message.params.contentChanges[0];
    if (change && typeof change.text === "string") {
      documents.set(uri, change.text);
      publishDiagnostics(uri, change.text);
    }
    return;
  }

  if (message.method === "textDocument/didClose") {
    const uri = message.params.textDocument.uri;
    documents.delete(uri);
    notify("textDocument/publishDiagnostics", { uri, diagnostics: [] });
    return;
  }

  if (message.method === "textDocument/completion") {
    const uri = message.params.textDocument.uri;
    const text = documents.get(uri) || "";
    const items = computeCompletions(text, message.params.position, uri);
    respond(message.id, {
      isIncomplete: false,
      items
    });
    return;
  }

  if (Object.prototype.hasOwnProperty.call(message, "id")) {
    respond(message.id, null);
  }
}

function publishDiagnostics(uri, text) {
  const diagnostics = analyzeText(text, uri);
  notify("textDocument/publishDiagnostics", {
    uri,
    diagnostics
  });
}

function respond(id, result) {
  send({
    jsonrpc: "2.0",
    id,
    result
  });
}

function notify(method, params) {
  send({
    jsonrpc: "2.0",
    method,
    params
  });
}

function log(message) {
  notify("window/logMessage", {
    type: 3,
    message
  });
}

function send(payload) {
  const json = JSON.stringify(payload);
  const length = Buffer.byteLength(json, "utf8");
  process.stdout.write(`Content-Length: ${length}\r\n\r\n${json}`);
}
