{
  description = "λsh: a functional shell with composable environments";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

  outputs = { self, nixpkgs }:
    let
      systems = [ "aarch64-darwin" "x86_64-darwin" "aarch64-linux" "x86_64-linux" ];
      forAllSystems = f: nixpkgs.lib.genAttrs systems (system: f nixpkgs.legacyPackages.${system});
      # Isocline, patched for a shell (nix/isocline.patch):
      # - Ctrl-C gives "\x03". Without it, Ctrl-C and Ctrl-D both give no
      #   line, and Ctrl-C on an empty line would quit.
      # - Keys typed while a command ran are kept: TCSAFLUSH dropped them.
      # - On one line, a linefeed ends the line. The driver turns an Enter
      #   typed while a command ran into one.
      # - Without a terminal (a pipe, TERM=dumb), the prompt reads markup and
      #   is flushed. An empty line is a line, end of input is none (not empty
      #   lines for ever), and a signal such as SIGCHLD is not end of input.
      #   Ctrl-C, there SIGINT, gives "\x03" too: Emacs M-x shell is dumb.
      haskellPackagesFor = pkgs: pkgs.haskellPackages.override {
        overrides = _: super: {
          isocline = pkgs.haskell.lib.appendPatch super.isocline ./nix/isocline.patch;
        };
      };
    in {
      packages = forAllSystems (pkgs: rec {
        # Tests stay in the devShell: the pty suites need a terminal and
        # expect, and repl-tests spawn real programs. The job-control spike
        # is a test program, so it stays off PATH.
        lambdash = with pkgs.haskell.lib.compose;
          pkgs.lib.pipe ((haskellPackagesFor pkgs).callCabal2nix "lambdash" ./. { }) [
            dontCheck
            justStaticExecutables
            (overrideCabal (_: {
              postInstall = "rm $out/bin/spike-jobs";
              mainProgram = "lambdash";
            }))
          ];
        default = lambdash;

        # The VS Code extension in editors/vscode, with its node_modules.
        # For home-manager: programs.vscode.profiles.default.extensions.
        # It starts `lambdash --lsp` from PATH unless lambdash.path says
        # otherwise.
        vscode-extension =
          let
            withModules = pkgs.buildNpmPackage {
              pname = "lambdash-vscode-modules";
              version = "0.1.0";
              src = ./editors/vscode;
              npmDepsHash = "sha256-kb0kpYijKEHFtPvnwJuyzUCJ2tT0+OCFCtB9RL+bGZU=";
              dontNpmBuild = true;
              installPhase = "cp -r . $out";
            };
          in
          pkgs.vscode-utils.buildVscodeExtension {
            pname = "lambdash";
            version = "0.1.0";
            src = withModules;
            sourceRoot = withModules.name;
            vscodeExtPublisher = "lillycham";
            vscodeExtName = "lambdash";
            vscodeExtUniqueId = "lillycham.lambdash";
          };
      });

      devShells = forAllSystems (pkgs: {
        default = pkgs.mkShell {
          packages = with pkgs; [
            # From nixpkgs, so cabal never fetches from Hackage.
            ((haskellPackagesFor pkgs).ghcWithPackages (p: [ p.async p.isocline p.lsp p.megaparsec p.optparse-applicative ]))
            cabal-install
            expect
            haskell-language-server
          ];
        };
      });
    };
}
