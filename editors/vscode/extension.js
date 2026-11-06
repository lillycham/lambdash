// Connects VS Code to λsh: the language server for checking, and terminals
// for running. All of the checking is in `lambdash --lsp`.
const path = require("path");
const vscode = require("vscode");
const { LanguageClient } = require("vscode-languageclient/node");

let client;
// The terminal that Send Selection types into: one λsh prompt, kept between
// sends, so a file can build up a session line by line.
let prompt;
// The terminal of the last Run File, closed when the next run starts.
let lastRun;

function program() {
  return vscode.workspace.getConfiguration("lambdash").get("path") || "lambdash";
}

function start() {
  const command = program();
  // No transport: the client then talks over stdio and adds no --stdio
  // argument, which lambdash does not take.
  const server = { command, args: ["--lsp"] };
  client = new LanguageClient("lambdash", "λsh", { run: server, debug: server }, {
    documentSelector: [{ language: "lambdash" }],
  });
  return client.start().catch((err) => {
    vscode.window.showErrorMessage(`λsh: could not start "${command} --lsp": ${err.message}. Set lambdash.path to the program.`);
  });
}

async function restart() {
  if (client) await client.stop();
  await start();
}

// Where a file's commands start: its workspace folder, as a person would
// run it from the project, or else its own directory. Imports start at the
// file's directory either way.
function directoryFor(uri) {
  const folder = vscode.workspace.getWorkspaceFolder(uri);
  return folder ? folder.uri.fsPath : path.dirname(uri.fsPath);
}

// λsh: Run File. The terminal's process is lambdash itself, so no shell
// reads the path, and the terminal shows the script's exit status.
async function runFile(uri) {
  const editor = vscode.window.activeTextEditor;
  const document = uri instanceof vscode.Uri
    ? await vscode.workspace.openTextDocument(uri)
    : editor && editor.document;
  if (!document || document.languageId !== "lambdash") {
    vscode.window.showErrorMessage("λsh: open a .lsh file to run it.");
    return;
  }
  if (document.isUntitled) {
    vscode.window.showErrorMessage("λsh: save the file before running it.");
    return;
  }
  if (document.isDirty) await document.save();
  if (lastRun) lastRun.dispose();
  lastRun = vscode.window.createTerminal({
    name: `λsh: ${path.basename(document.uri.fsPath)}`,
    shellPath: program(),
    shellArgs: [document.uri.fsPath],
    cwd: directoryFor(document.uri),
    env: terminalEnv,
  });
  lastRun.show(true);
}

// VS Code's terminal draws 24-bit colour. Saying so also stops isocline's
// colour query at start, which on macOS reads a key sent before the reply
// and drops it: the first send to a new prompt then lost its first letter.
const terminalEnv = { COLORTERM: "truecolor" };

function promptTerminal(uri) {
  if (prompt && prompt.exitStatus === undefined) return prompt;
  prompt = vscode.window.createTerminal({
    name: "λsh",
    shellPath: program(),
    cwd: uri && uri.scheme === "file" ? directoryFor(uri) : undefined,
    env: terminalEnv,
  });
  return prompt;
}

// λsh: Send Selection. The selection, or else the line under the cursor,
// goes to the λsh prompt as if typed. With no selection the cursor moves to
// the next line, so Shift+Enter steps through a file. A `do` block that
// spans lines continues at the prompt, as when typed.
function sendSelection() {
  const editor = vscode.window.activeTextEditor;
  if (!editor) return;
  const selection = editor.selection;
  const text = selection.isEmpty
    ? editor.document.lineAt(selection.active.line).text
    : editor.document.getText(selection);
  sendToPrompt(editor.document.uri, text);
  if (selection.isEmpty) {
    const next = Math.min(selection.active.line + 1, editor.document.lineCount - 1);
    const position = new vscode.Position(next, 0);
    editor.selection = new vscode.Selection(position, position);
    editor.revealRange(new vscode.Range(position, position));
  }
}

// The ▶ of a code lens: its lines go to the prompt, as Send Selection sends
// them. The language server gives the file and the lines, from 0.
async function sendLines(uri, first, last) {
  const document = await vscode.workspace.openTextDocument(vscode.Uri.parse(uri));
  const end = document.lineAt(last).range.end;
  sendToPrompt(document.uri, document.getText(new vscode.Range(first, 0, end.line, end.character)));
}

function sendToPrompt(uri, text) {
  const terminal = promptTerminal(uri);
  terminal.show(true);
  terminal.sendText(text.replace(/\r\n/g, "\n"), true);
}

function openPrompt() {
  const editor = vscode.window.activeTextEditor;
  promptTerminal(editor && editor.document.uri).show();
}

function activate(context) {
  context.subscriptions.push(
    vscode.commands.registerCommand("lambdash.restart", restart),
    vscode.commands.registerCommand("lambdash.runFile", runFile),
    vscode.commands.registerCommand("lambdash.sendSelection", sendSelection),
    vscode.commands.registerCommand("lambdash.sendLines", sendLines),
    vscode.commands.registerCommand("lambdash.openPrompt", openPrompt),
    vscode.window.onDidCloseTerminal((closed) => {
      if (closed === prompt) prompt = undefined;
      if (closed === lastRun) lastRun = undefined;
    }),
    vscode.workspace.onDidChangeConfiguration((event) => {
      if (event.affectsConfiguration("lambdash.path")) restart();
    }),
  );
  return start();
}

function deactivate() {
  return client ? client.stop() : undefined;
}

module.exports = { activate, deactivate };
