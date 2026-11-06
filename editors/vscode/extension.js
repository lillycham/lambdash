// Starts `lambdash --lsp` for .lsh files. All of the checking is in the
// server: this only connects VS Code to it.
const vscode = require("vscode");
const { LanguageClient } = require("vscode-languageclient/node");

let client;

function start() {
  const command = vscode.workspace.getConfiguration("lambdash").get("path") || "lambdash";
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

function activate(context) {
  context.subscriptions.push(
    vscode.commands.registerCommand("lambdash.restart", restart),
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
