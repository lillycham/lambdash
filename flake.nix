{
  description = "λsh: a functional shell with composable environments";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

  outputs = { self, nixpkgs }:
    let
      systems = [ "aarch64-darwin" "x86_64-darwin" "aarch64-linux" "x86_64-linux" ];
      forAllSystems = f: nixpkgs.lib.genAttrs systems (system: f nixpkgs.legacyPackages.${system});
    in {
      packages = forAllSystems (pkgs: rec {
        # Tests stay in the devShell: the pty suites need a terminal and
        # expect, and repl-tests spawn real programs. The job-control spike
        # is a test program, so it stays off PATH.
        lambdash = with pkgs.haskell.lib.compose;
          pkgs.lib.pipe (pkgs.haskellPackages.callCabal2nix "lambdash" ./. { }) [
            dontCheck
            justStaticExecutables
            (overrideCabal (_: {
              postInstall = "rm $out/bin/spike-jobs";
              mainProgram = "lambdash";
            }))
          ];
        default = lambdash;
      });

      devShells = forAllSystems (pkgs: {
        default = pkgs.mkShell {
          packages = with pkgs; [
            # From nixpkgs, so cabal never fetches from Hackage.
            (haskellPackages.ghcWithPackages (p: [ p.async p.haskeline p.megaparsec ]))
            cabal-install
            expect
            haskell-language-server
          ];
        };
      });
    };
}
