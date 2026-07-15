"use strict";

const ERROR = 1;
const WARNING = 2;
const INFORMATION = 3;

const CompletionItemKind = {
  Text: 1,
  Method: 2,
  Function: 3,
  Constructor: 4,
  Field: 5,
  Variable: 6,
  Class: 7,
  Interface: 8,
  Module: 9,
  Property: 10,
  Keyword: 14,
  Snippet: 15,
  TypeParameter: 25
};

const primitiveTypes = [
  "void", "bool", "char", "uchar", "int", "uint", "int8", "int16",
  "int32", "int64", "uint8", "uint16", "uint32", "uint64", "uint128",
  "isize", "usize", "float", "float32", "float64", "any", "vec2",
  "vec3", "vec4", "dyn"
];

const qualifiers = [
  "const", "constptr", "constref", "readonly", "nomove", "public",
  "private", "static", "dynamic", "shared", "allocator"
];

const controlKeywords = [
  "ret", "if", "elif", "else", "loop", "in", "break", "continue",
  "option", "default", "throw", "defer", "drop"
];

const declarationKeywords = [
  "struct", "trait", "protocol", "enum", "flags", "tagged", "macro",
  "attribute", "constructor", "destructor", "operator", "import",
  "export", "include", "from", "with"
];

const operatorKeywords = [
  "ref", "deref", "move", "new", "cast", "unsafe_cast", "sizeof",
  "typeof", "as", "is", "strong_count"
];

const builtinFunctions = [
  "print", "input_int", "input_string", "clear_screen", "flush_output",
  "sleep_ms", "enable_raw_input", "disable_raw_input", "read_key",
  "core_print", "core_println", "core_print_char", "core_print_i32",
  "core_print_i64", "core_print_f64", "core_read_i32", "core_read_i64",
  "core_read_string", "printf", "scanf", "getchar"
];

const topLevelSnippets = [
  {
    label: "main",
    kind: CompletionItemKind.Snippet,
    detail: "C* main function",
    insertTextFormat: 2,
    insertText: "main() :: int32 {\n    ret 0;\n}"
  },
  {
    label: "struct",
    kind: CompletionItemKind.Snippet,
    detail: "C* struct",
    insertTextFormat: 2,
    insertText: "struct ${1:Name} {\n    ${2:int32 value;}\n}"
  },
  {
    label: "trait",
    kind: CompletionItemKind.Snippet,
    detail: "C* trait",
    insertTextFormat: 2,
    insertText: "trait ${1:Name} {\n    ${2:method}(${3}) :: ${4:void};\n}"
  },
  {
    label: "import from",
    kind: CompletionItemKind.Snippet,
    detail: "C* import-from block",
    insertTextFormat: 2,
    insertText: "import from \"${1:std:crt}\" {\n    ${2:printf}(const char* fmt, ...) :: int32;\n}"
  }
];

const openToClose = { "{": "}", "[": "]", "(": ")" };
const closeToOpen = { "}": "{", "]": "[", ")": "(" };
const typeKeywords = new Set(primitiveTypes);
const qualifierSet = new Set(qualifiers);
const reservedWords = new Set([
  ...primitiveTypes,
  ...qualifiers,
  ...controlKeywords,
  ...declarationKeywords,
  ...operatorKeywords,
  "true", "false", "nil", "self", "async", "await", "except", "noexcept",
  "onexcept", "state", "default", "scope_exit", "opened", "closed", "idle",
  "sending", "failed"
]);

function analyzeText(text, uri = "file:///unknown.cstar") {
  const document = buildDocument(text, uri);
  const diagnostics = [];
  const add = (code, message, offset, length = 1, severity = ERROR) => {
    diagnostics.push({
      severity,
      code,
      source: "cstar-static",
      message,
      range: makeRange(document.lineStarts, offset, Math.max(length, 1))
    });
  };

  for (const diagnostic of document.lexDiagnostics) {
    add(diagnostic.code, diagnostic.message, diagnostic.offset, diagnostic.length, diagnostic.severity);
  }

  checkDelimiters(document.clean, add);
  checkDuplicateDeclarations(document.symbols, add);
  checkLoopControl(document.tokens, add);
  checkFunctionFlow(document.tokens, add);
  checkPointerMarkers(document.tokens, add);
  checkStructConformance(document.symbols, add);
  checkUnknownSimpleTypes(document, add);
  checkInvalidQualifierCombinations(document.symbols, add);
  checkOwnershipFlow(document.tokens, document.symbols, add);
  checkProtocolDynamicAssignment(document.tokens, add);

  return diagnostics;
}

function buildDocument(text, uri = "file:///unknown.cstar") {
  const lineStarts = computeLineStarts(text);
  const lexDiagnostics = [];
  const addLexDiagnostic = (code, message, offset, length = 1, severity = ERROR) => {
    lexDiagnostics.push({ code, message, offset, length, severity });
  };
  const clean = maskTrivia(text, addLexDiagnostic);
  const tokens = tokenize(clean);
  const symbols = collectSymbols(tokens, clean);
  return { uri, text, clean, lineStarts, lexDiagnostics, tokens, symbols };
}

function computeCompletions(text, position, uri = "file:///unknown.cstar") {
  const document = buildDocument(text, uri);
  const offset = offsetAt(document.lineStarts, position);
  const memberContext = getMemberContext(document, offset);
  if (memberContext) {
    return completeMember(document, memberContext);
  }

  const items = [];
  addKeywordItems(items);
  addSymbolItems(items, document.symbols);
  addBuiltins(items);

  if (isLikelyTopLevel(document.clean, offset)) {
    items.push(...topLevelSnippets);
  }

  return dedupeCompletionItems(items);
}

function addKeywordItems(items) {
  for (const label of declarationKeywords) {
    items.push({ label, kind: CompletionItemKind.Keyword, detail: "C* declaration keyword" });
  }
  for (const label of controlKeywords) {
    items.push({ label, kind: CompletionItemKind.Keyword, detail: "C* control keyword" });
  }
  for (const label of operatorKeywords) {
    items.push({ label, kind: CompletionItemKind.Keyword, detail: "C* operator keyword" });
  }
  for (const label of qualifiers) {
    items.push({ label, kind: CompletionItemKind.Keyword, detail: "C* qualifier" });
  }
  for (const label of primitiveTypes) {
    items.push({ label, kind: CompletionItemKind.Keyword, detail: "C* primitive type" });
  }
  items.push({ label: "new?", kind: CompletionItemKind.Keyword, detail: "C* fallible allocation proposal" });
}

function addSymbolItems(items, symbols) {
  for (const type of symbols.types.values()) {
    items.push({
      label: type.name,
      kind: type.kind === "trait" || type.kind === "protocol" ? CompletionItemKind.Interface : CompletionItemKind.Class,
      detail: `C* ${type.kind}`
    });
  }
  for (const fn of symbols.functions.values()) {
    items.push({
      label: fn.name,
      kind: fn.kind === "constructor" ? CompletionItemKind.Constructor : CompletionItemKind.Function,
      detail: fn.signature || "C* function"
    });
  }
  for (const variable of symbols.variables.values()) {
    items.push({
      label: variable.name,
      kind: CompletionItemKind.Variable,
      detail: variable.type ? `${variable.type} variable` : "C* variable"
    });
  }
  for (const alias of symbols.modules.values()) {
    items.push({ label: alias.name, kind: CompletionItemKind.Module, detail: "C* module alias" });
  }
}

function addBuiltins(items) {
  for (const label of builtinFunctions) {
    items.push({ label, kind: CompletionItemKind.Function, detail: "C* builtin/runtime helper" });
  }
}

function completeMember(document, context) {
  const { symbols } = document;
  const items = [];

  if (context.operator === "::") {
    const type = symbols.types.get(context.receiver);
    if (type) {
      for (const method of type.methods) {
        if (method.isStatic || type.kind === "enum" || type.kind === "tagged") {
          items.push({
            label: method.name,
            kind: method.kind === "variant" ? CompletionItemKind.Field : CompletionItemKind.Method,
            detail: method.detail || `${context.receiver}::${method.name}`
          });
        }
      }
      for (const field of type.fields) {
        if (type.kind === "enum" || type.kind === "tagged") {
          items.push({ label: field.name, kind: CompletionItemKind.Field, detail: `${context.receiver} variant` });
        }
      }
    }
    return dedupeCompletionItems(items);
  }

  const variable = symbols.variables.get(context.receiver);
  const typeName = variable ? variable.type : context.receiver;
  const type = symbols.types.get(stripTypeDecorators(typeName || ""));
  if (type) {
    for (const field of type.fields) {
      items.push({ label: field.name, kind: CompletionItemKind.Field, detail: field.type ? `${field.type} field` : "field" });
    }
    for (const method of type.methods) {
      if (!method.isStatic) {
        items.push({ label: method.name, kind: CompletionItemKind.Method, detail: method.signature || "method" });
      }
    }
  }

  const module = symbols.modules.get(context.receiver);
  if (module) {
    for (const fn of symbols.functions.values()) {
      if (fn.visibility === "public") {
        items.push({ label: fn.name, kind: CompletionItemKind.Function, detail: "public module function" });
      }
    }
  }

  return dedupeCompletionItems(items);
}

function getMemberContext(document, offset) {
  const before = document.clean.slice(0, offset);
  const match = /([A-Za-z_][A-Za-z0-9_]*)\s*(::|\.)\s*[A-Za-z0-9_]*$/.exec(before);
  if (!match) return null;
  return { receiver: match[1], operator: match[2] };
}

function isLikelyTopLevel(clean, offset) {
  let depth = 0;
  for (let i = 0; i < offset; i += 1) {
    if (clean[i] === "{") depth += 1;
    if (clean[i] === "}") depth = Math.max(0, depth - 1);
  }
  return depth === 0;
}

function dedupeCompletionItems(items) {
  const byLabel = new Map();
  for (const item of items) {
    if (!byLabel.has(item.label)) byLabel.set(item.label, item);
  }
  return [...byLabel.values()];
}

function computeLineStarts(text) {
  const starts = [0];
  for (let i = 0; i < text.length; i += 1) {
    if (text.charCodeAt(i) === 10) starts.push(i + 1);
  }
  return starts;
}

function offsetAt(lineStarts, position) {
  const line = Math.max(0, Math.min(position.line, lineStarts.length - 1));
  return lineStarts[line] + Math.max(0, position.character);
}

function positionAt(lineStarts, offset) {
  let low = 0;
  let high = lineStarts.length - 1;
  while (low <= high) {
    const mid = (low + high) >> 1;
    if (lineStarts[mid] <= offset) low = mid + 1;
    else high = mid - 1;
  }
  const line = Math.max(0, high);
  return { line, character: Math.max(0, offset - lineStarts[line]) };
}

function makeRange(lineStarts, offset, length) {
  return {
    start: positionAt(lineStarts, offset),
    end: positionAt(lineStarts, offset + length)
  };
}

function preserveNewlines(fragment) {
  return fragment.replace(/[^\r\n]/g, " ");
}

function maskTrivia(text, addDiagnostic) {
  let out = "";
  let i = 0;

  while (i < text.length) {
    const ch = text[i];
    const next = text[i + 1];

    if (ch === "/" && next === "/") {
      const start = i;
      i += 2;
      while (i < text.length && text[i] !== "\n") i += 1;
      out += preserveNewlines(text.slice(start, i));
      continue;
    }

    if (ch === "/" && next === "*") {
      const start = i;
      i += 2;
      while (i < text.length && !(text[i] === "*" && text[i + 1] === "/")) i += 1;
      if (i >= text.length) {
        addDiagnostic("CSTA1001", "Unterminated block comment.", start, 2);
        out += preserveNewlines(text.slice(start));
        break;
      }
      i += 2;
      out += preserveNewlines(text.slice(start, i));
      continue;
    }

    if (ch === "\"") {
      const start = i;
      i += 1;
      let closed = false;
      while (i < text.length) {
        if (text[i] === "\\") {
          i += 2;
          continue;
        }
        if (text[i] === "\"") {
          i += 1;
          closed = true;
          break;
        }
        if (text[i] === "\n" || text[i] === "\r") break;
        i += 1;
      }
      if (!closed) addDiagnostic("CSTA1002", "Unterminated string literal.", start, 1);
      out += preserveNewlines(text.slice(start, i));
      continue;
    }

    if (ch === "'") {
      const start = i;
      i += 1;
      let closed = false;
      while (i < text.length) {
        if (text[i] === "\\") {
          i += 2;
          continue;
        }
        if (text[i] === "'") {
          i += 1;
          closed = true;
          break;
        }
        if (text[i] === "\n" || text[i] === "\r") break;
        i += 1;
      }
      if (!closed) addDiagnostic("CSTA1003", "Unterminated character literal.", start, 1);
      out += preserveNewlines(text.slice(start, i));
      continue;
    }

    out += ch;
    i += 1;
  }

  return out;
}

function tokenize(clean) {
  const tokens = [];
  const pattern = /\.=|:=|::|->|=>|===|==|!=|<=|>=|\+\+|--|\.\.\.|[A-Za-z_][A-Za-z0-9_]*|\d+\.\d+|\d+|[{}()[\],;:.<>*^&=+\-/|!?~%]/g;
  let match;
  while ((match = pattern.exec(clean)) !== null) {
    tokens.push({ text: match[0], offset: match.index });
  }
  return tokens;
}

function collectSymbols(tokens, clean) {
  const symbols = {
    types: new Map(),
    functions: new Map(),
    variables: new Map(),
    modules: new Map()
  };

  for (let i = 0; i < tokens.length; i += 1) {
    const token = tokens[i];
    if (token.text === "include") {
      const aliasIndex = findNextText(tokens, i, "as", 20);
      if (aliasIndex !== -1 && isIdentifier(tokens[aliasIndex + 1]?.text)) {
        symbols.modules.set(tokens[aliasIndex + 1].text, {
          name: tokens[aliasIndex + 1].text,
          offset: tokens[aliasIndex + 1].offset
        });
      }
      continue;
    }

    if (token.text === "struct" || token.text === "trait" || token.text === "protocol" ||
        token.text === "enum" || token.text === "tagged" ||
        (token.text === "enum" && tokens[i - 1]?.text === "flags")) {
      const nameToken = nextIdentifierToken(tokens, i + 1);
      if (!nameToken) continue;
      const kind = token.text;
      const body = collectBody(tokens, tokens.indexOf(nameToken) + 1);
      const typeInfo = {
        name: nameToken.text,
        kind,
        offset: nameToken.offset,
        fields: [],
        methods: [],
        traits: collectTraits(tokens, tokens.indexOf(nameToken) + 1, body.openIndex),
        body
      };
      if (body.openIndex !== -1 && body.closeIndex !== -1) {
        collectTypeMembers(tokens, body.openIndex + 1, body.closeIndex, typeInfo);
      }
      symbols.types.set(typeInfo.name, typeInfo);
      continue;
    }

    if (isFunctionStart(tokens, i)) {
      const name = token.text;
      const signature = clean.slice(token.offset, tokens[Math.min(tokens.length - 1, i + 8)].offset).split("{")[0].trim();
      symbols.functions.set(name, {
        name,
        kind: "function",
        offset: token.offset,
        signature,
        visibility: inferVisibility(tokens, i)
      });
      continue;
    }

    const variable = parseVariableDeclaration(tokens, i);
    if (variable) {
      symbols.variables.set(variable.name, variable);
      i = variable.endIndex;
    }
  }

  return symbols;
}

function collectTraits(tokens, startIndex, endIndex) {
  const traits = [];
  if (endIndex === -1) endIndex = Math.min(tokens.length, startIndex + 20);
  const withIndex = findNextText(tokens, startIndex, "with", endIndex - startIndex);
  if (withIndex === -1 || withIndex >= endIndex) return traits;
  for (let i = withIndex + 1; i < endIndex; i += 1) {
    if (isIdentifier(tokens[i].text)) traits.push(tokens[i].text);
  }
  return traits;
}

function collectTypeMembers(tokens, start, end, typeInfo) {
  let depth = 0;
  for (let i = start; i < end; i += 1) {
    const token = tokens[i];
    if (token.text === "{") {
      depth += 1;
      continue;
    }
    if (token.text === "}") {
      depth -= 1;
      continue;
    }
    if (depth !== 0) continue;

    if (token.text === "constructor" || token.text === "destructor" || token.text === "operator") {
      typeInfo.methods.push({
        name: token.text === "operator" ? `operator ${tokens[i + 1]?.text || ""}`.trim() : token.text,
        kind: token.text,
        isStatic: false,
        offset: token.offset
      });
      i = skipMemberSignatureOrBody(tokens, i, end);
      continue;
    }

    if (token.text === "static" && isIdentifier(tokens[i + 1]?.text)) {
      typeInfo.methods.push({
        name: tokens[i + 1].text,
        kind: "method",
        isStatic: true,
        offset: tokens[i + 1].offset
      });
      i = skipMemberSignatureOrBody(tokens, i + 1, end);
      continue;
    }

    if (isIdentifier(token.text) && tokens[i + 1]?.text === "(") {
      typeInfo.methods.push({ name: token.text, kind: "method", isStatic: false, offset: token.offset });
      i = skipMemberSignatureOrBody(tokens, i, end);
      continue;
    }

    const field = parseVariableDeclaration(tokens, i);
    if (field) {
      typeInfo.fields.push(field);
      i = field.endIndex;
      continue;
    }

    if ((typeInfo.kind === "enum" || typeInfo.kind === "tagged") && isIdentifier(token.text) && !reservedWords.has(token.text)) {
      typeInfo.fields.push({ name: token.text, type: typeInfo.name, offset: token.offset });
      typeInfo.methods.push({ name: token.text, kind: "variant", isStatic: true, offset: token.offset });
    }
  }
}

function skipMemberSignatureOrBody(tokens, start, end) {
  for (let i = start; i < end; i += 1) {
    if (tokens[i].text === ";") return i;
    if (tokens[i].text === "{") {
      let depth = 0;
      for (let j = i; j < end; j += 1) {
        if (tokens[j].text === "{") depth += 1;
        if (tokens[j].text === "}") {
          depth -= 1;
          if (depth === 0) return j;
        }
      }
      return end;
    }
  }
  return start;
}

function parseVariableDeclaration(tokens, i) {
  let index = i;
  const qualifiers = [];
  while (qualifierSet.has(tokens[index]?.text)) {
    qualifiers.push({ text: tokens[index].text, offset: tokens[index].offset });
    index += 1;
  }
  const typeStart = tokens[index];
  if (!typeStart) return null;
  let type = "";
  if (typeStart.text === "dynamic" && isTypeName(tokens[index + 1]?.text)) {
    type = `dynamic ${tokens[index + 1].text}`;
    index += 2;
  } else if (!isTypeName(typeStart.text) && isProtocolStateType(tokens, index)) {
    type = `${typeStart.text} ${tokens[index + 1].text}`;
    index += 2;
  } else if (isTypeName(typeStart.text)) {
    type = typeStart.text;
    index += 1;
  } else {
    return null;
  }

  if (tokens[index]?.text === "[") {
    const closeIndex = findBalancedToken(tokens, index, "[", "]");
    if (closeIndex !== -1) {
      type += tokens.slice(index, closeIndex + 1).map(token => token.text).join("");
      index = closeIndex + 1;
    }
  }

  while (tokens[index] && ["*", "^", "&"].includes(tokens[index].text)) {
    type += tokens[index].text;
    index += 1;
  }
  if (tokens[index]?.text === "?") {
    type += "?";
    index += 1;
  }
  const nameToken = tokens[index];
  if (!nameToken || !isIdentifier(nameToken.text) || reservedWords.has(nameToken.text)) return null;
  if (tokens[index + 1]?.text === "(") return null;
  let endIndex = index;
  while (tokens[endIndex] && ![";", ",", "}", "{"].includes(tokens[endIndex].text)) endIndex += 1;
  return {
    name: nameToken.text,
    type,
    qualifiers,
    offset: nameToken.offset,
    endIndex
  };
}

function isProtocolStateType(tokens, index) {
  return isIdentifier(tokens[index]?.text) &&
    /^[a-z]/.test(tokens[index].text) &&
    isTypeName(tokens[index + 1]?.text);
}

function findBalancedToken(tokens, start, open, close) {
  let depth = 0;
  for (let i = start; i < tokens.length; i += 1) {
    if (tokens[i].text === open) depth += 1;
    if (tokens[i].text === close) {
      depth -= 1;
      if (depth === 0) return i;
    }
  }
  return -1;
}

function checkDelimiters(clean, add) {
  const stack = [];
  for (let i = 0; i < clean.length; i += 1) {
    const ch = clean[i];
    if (openToClose[ch]) {
      stack.push({ ch, offset: i });
      continue;
    }
    if (closeToOpen[ch]) {
      const expectedOpen = closeToOpen[ch];
      const top = stack[stack.length - 1];
      if (!top || top.ch !== expectedOpen) add("CSTA1100", `Unmatched closing delimiter '${ch}'.`, i, 1);
      else stack.pop();
    }
  }
  for (const item of stack) add("CSTA1101", `Unclosed delimiter '${item.ch}'.`, item.offset, 1);
}

function checkDuplicateDeclarations(symbols, add) {
  const seen = new Map();
  for (const bucket of [symbols.types, symbols.functions]) {
    for (const symbol of bucket.values()) {
      if (seen.has(symbol.name)) {
        const previous = seen.get(symbol.name);
        add("CSTA1200", `Duplicate top-level declaration '${symbol.name}'; first seen as ${previous.kind}.`, symbol.offset, symbol.name.length, WARNING);
      } else {
        seen.set(symbol.name, symbol);
      }
    }
  }
}

function checkLoopControl(tokens, add) {
  const contextStack = [];
  let pendingLoop = false;
  for (const token of tokens) {
    if (token.text === "loop") {
      pendingLoop = true;
      continue;
    }
    if (token.text === "{") {
      contextStack.push(pendingLoop ? "loop" : "block");
      pendingLoop = false;
      continue;
    }
    if (token.text === "}") {
      contextStack.pop();
      pendingLoop = false;
      continue;
    }
    if ((token.text === "break" || token.text === "continue") && !contextStack.includes("loop")) {
      add("CSTA1300", `'${token.text}' can only be used inside a loop.`, token.offset, token.text.length);
    }
  }
}

function checkFunctionFlow(tokens, add) {
  const braceStack = [];
  for (let i = 0; i < tokens.length; i += 1) {
    const token = tokens[i];
    if (token.text === "{") {
      braceStack.push(isFunctionBlock(tokens, i) ? "function" : "block");
      continue;
    }
    if (token.text === "}") {
      braceStack.pop();
      continue;
    }
    if (token.text === "ret" && !braceStack.includes("function")) {
      add("CSTA1301", "'ret' can only be used inside a function or method body.", token.offset, token.text.length);
    }
  }
}

function isFunctionBlock(tokens, openBraceIndex) {
  let start = openBraceIndex - 1;
  while (start >= 0 && ![";", "{", "}"].includes(tokens[start].text)) start -= 1;
  const segment = tokens.slice(start + 1, openBraceIndex).map(token => token.text);
  if (segment.length === 0) return false;
  if (["if", "elif", "else", "loop", "for", "option", "match", "case", "default"].includes(segment[0])) {
    return false;
  }
  if (segment.includes("constructor") || segment.includes("destructor") || segment.includes("operator")) {
    return true;
  }
  const parenIndex = segment.indexOf("(");
  if (parenIndex <= 0 || !segment.includes(")")) return false;
  const callable = segment[parenIndex - 1];
  return Boolean(isIdentifier(callable) && !reservedWords.has(callable));
}

function checkPointerMarkers(tokens, add) {
  for (let i = 0; i < tokens.length - 2; i += 1) {
    if (!isTypeName(tokens[i].text)) continue;
    let sawStar = false;
    let sawCaret = false;
    let j = i + 1;
    while (tokens[j] && (tokens[j].text === "*" || tokens[j].text === "^")) {
      sawStar = sawStar || tokens[j].text === "*";
      sawCaret = sawCaret || tokens[j].text === "^";
      j += 1;
    }
    if (sawStar && sawCaret) {
      add("CSTA1400", "Do not mix shared '*' and unique '^' pointer markers in the same type.", tokens[i + 1].offset, tokens[j - 1].offset - tokens[i + 1].offset + 1);
    }
  }
}

function checkStructConformance(symbols, add) {
  for (const type of symbols.types.values()) {
    if (type.kind !== "struct" || type.traits.length === 0) continue;
    for (const traitName of type.traits) {
      const trait = symbols.types.get(traitName);
      if (!trait || trait.kind !== "trait") continue;
      for (const required of trait.methods) {
        if (!type.methods.some(method => method.name === required.name)) {
          add("CSTA1500", `Struct '${type.name}' does not implement trait method '${required.name}' from '${traitName}'.`, type.offset, type.name.length, WARNING);
        }
      }
    }
  }
}

function checkUnknownSimpleTypes(document, add) {
  const { symbols, tokens } = document;
  for (let i = 0; i < tokens.length - 1; i += 1) {
    const token = tokens[i];
    if (!looksLikeTypeUsage(tokens, i)) continue;
    const base = stripTypeDecorators(token.text);
    if (typeKeywords.has(base) || symbols.types.has(base) || base === "dynamic") continue;
    if (/^[A-Z]/.test(base)) {
      add("CSTA1600", `Unknown type '${base}'.`, token.offset, token.text.length, WARNING);
    }
  }
}

function checkInvalidQualifierCombinations(symbols, add) {
  for (const variable of symbols.variables.values()) {
    const qualifierTexts = new Set((variable.qualifiers || []).map(item => item.text));
    const type = variable.type || "";
    const isPointerLike = /[*^&]/.test(type);
    const isUnique = type.includes("^");
    if (qualifierTexts.has("constref") && !type.includes("&")) {
      const q = variable.qualifiers.find(item => item.text === "constref");
      add("CSTA2101", "`constref` requires a reference type like `T&`.", q?.offset || variable.offset, "constref".length);
    }
    for (const qualifier of ["constptr", "readonly"]) {
      if (qualifierTexts.has(qualifier) && !isPointerLike) {
        const q = variable.qualifiers.find(item => item.text === qualifier);
        add("CSTA2102", `\`${qualifier}\` requires a pointer/reference type.`, q?.offset || variable.offset, qualifier.length);
      }
    }
    if (qualifierTexts.has("nomove") && !isUnique) {
      const q = variable.qualifiers.find(item => item.text === "nomove");
      add("CSTA2103", "`nomove` is only meaningful on unique `^` ownership.", q?.offset || variable.offset, "nomove".length);
    }
  }
}

function checkOwnershipFlow(tokens, symbols, add) {
  const moved = new Map();
  const variableTypes = new Map();
  for (const variable of symbols.variables.values()) variableTypes.set(variable.name, variable.type || "");

  for (let i = 0; i < tokens.length; i += 1) {
    const token = tokens[i];
    if (token.text === ":=") {
      const lhs = previousIdentifier(tokens, i);
      const rhs = nextIdentifier(tokens, i);
      const lhsType = variableTypes.get(lhs?.text || "");
      if (lhsType && isPrimitiveNonPointer(lhsType)) {
        add("CSTA2200", "`:=` transfers ownership and cannot be used with primitive non-pointer values.", token.offset, token.text.length);
      }
      if (rhs) moved.set(rhs.text, token.offset);
      continue;
    }

    if (token.text === "=") {
      const lhs = previousIdentifier(tokens, i);
      const rhs = nextIdentifier(tokens, i);
      const lhsType = variableTypes.get(lhs?.text || "");
      const previous = tokens[i + 1]?.text;
      const rhsIndex = rhs ? tokens.indexOf(rhs) : -1;
      const rhsIsCall = rhsIndex !== -1 && tokens[rhsIndex + 1]?.text === "(";
      if (lhsType?.includes("^") && rhs && !rhsIsCall && previous !== "ref" && previous !== "new" && previous !== "move") {
        add("CSTA2202", "Unique `^` ownership cannot be copied with `=`; use `move`, `ref`, `new`, or `:=` as appropriate.", token.offset, token.text.length);
      }
      continue;
    }

    if (token.text === "move") {
      const rhs = nextIdentifier(tokens, i);
      if (rhs) moved.set(rhs.text, token.offset);
      continue;
    }

    if (isIdentifier(token.text) && moved.has(token.text)) {
      const prev = tokens[i - 1]?.text;
      if (prev === "move" || prev === ":=") continue;
      add("CSTA2201", `Use of moved value '${token.text}'.`, token.offset, token.text.length);
      moved.delete(token.text);
    }
  }
}

function checkProtocolDynamicAssignment(tokens, add) {
  for (const token of tokens) {
    if (token.text === ".=") {
      add("CSTA2300", "Dynamic protocol assignment `.=` is still a proposal surface.", token.offset, token.text.length);
    }
  }
}

function previousIdentifier(tokens, index) {
  for (let i = index - 1; i >= 0; i -= 1) {
    if (isIdentifier(tokens[i].text)) return tokens[i];
    if ([";", "{", "}"].includes(tokens[i].text)) return null;
  }
  return null;
}

function nextIdentifier(tokens, index) {
  for (let i = index + 1; i < tokens.length; i += 1) {
    if (isIdentifier(tokens[i].text)) return tokens[i];
    if ([";", "{", "}"].includes(tokens[i].text)) return null;
  }
  return null;
}

function isPrimitiveNonPointer(type) {
  const stripped = stripTypeDecorators(type);
  return typeKeywords.has(stripped) && !/[*^&?[\]]/.test(type);
}

function looksLikeTypeUsage(tokens, i) {
  const token = tokens[i];
  if (!isIdentifier(token.text)) return false;
  const next = tokens[i + 1]?.text;
  const prev = tokens[i - 1]?.text;
  if (prev === "." || prev === "::") return false;
  if (next === "*" || next === "^" || next === "&") return true;
  if (isIdentifier(next) && !reservedWords.has(next)) return true;
  return false;
}

function inferVisibility(tokens, i) {
  for (let j = Math.max(0, i - 3); j < i; j += 1) {
    if (tokens[j].text === "public" || tokens[j].text === "export") return "public";
    if (tokens[j].text === "private") return "private";
  }
  return "default";
}

function isFunctionStart(tokens, i) {
  const token = tokens[i];
  if (!isIdentifier(token?.text) || reservedWords.has(token.text)) return false;
  return tokens[i + 1]?.text === "(";
}

function collectBody(tokens, startIndex) {
  let openIndex = -1;
  for (let i = startIndex; i < tokens.length; i += 1) {
    if (tokens[i].text === ";") return { openIndex: -1, closeIndex: -1 };
    if (tokens[i].text === "{") {
      openIndex = i;
      break;
    }
  }
  if (openIndex === -1) return { openIndex: -1, closeIndex: -1 };
  let depth = 0;
  for (let i = openIndex; i < tokens.length; i += 1) {
    if (tokens[i].text === "{") depth += 1;
    if (tokens[i].text === "}") {
      depth -= 1;
      if (depth === 0) return { openIndex, closeIndex: i };
    }
  }
  return { openIndex, closeIndex: tokens.length };
}

function findNextText(tokens, start, text, limit) {
  const end = Math.min(tokens.length, start + limit + 1);
  for (let i = start; i < end; i += 1) {
    if (tokens[i].text === text) return i;
  }
  return -1;
}

function nextIdentifierToken(tokens, start) {
  for (let i = start; i < tokens.length; i += 1) {
    if (isIdentifier(tokens[i].text)) return tokens[i];
    if (tokens[i].text === "{" || tokens[i].text === ";") return null;
  }
  return null;
}

function isIdentifier(text) {
  return typeof text === "string" && /^[A-Za-z_][A-Za-z0-9_]*$/.test(text);
}

function isTypeName(text) {
  return isIdentifier(text) && (typeKeywords.has(text) || /^[A-Z]/.test(text) || text === "dynamic");
}

function stripTypeDecorators(type) {
  return String(type || "").replace(/[*^&?]+$/g, "").replace(/\[[^\]]*\]/g, "");
}

module.exports = {
  analyzeText,
  buildDocument,
  computeCompletions,
  ERROR,
  WARNING,
  INFORMATION
};
