# A flake for test/nix.exp. Its lock reuses the nixpkgs node of the main
# flake.lock, so Nix can fetch it by hash without resolving anything.
{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

  outputs = { self, nixpkgs }:
    let
      systems = [ "aarch64-darwin" "x86_64-darwin" "aarch64-linux" "x86_64-linux" ];
      forAllSystems = f: nixpkgs.lib.genAttrs systems (system: f nixpkgs.legacyPackages.${system});
    in {
      packages = forAllSystems (pkgs: {
        default = pkgs.writeShellScriptBin "lsh-hello" ''
          echo "hello from lsh-hello, LSH_HOOK=''${LSH_HOOK:-unset}"
        '';
      });
      devShells = forAllSystems (pkgs: {
        default = pkgs.mkShell {
          packages = [ self.packages.${pkgs.stdenv.hostPlatform.system}.default ];
          LSH_ATTR = "from-attr";
          shellHook = ''
            export LSH_HOOK=ran
            lsh_one() { :; }
            lsh_two() { :; }
            echo "lsh fixture hook ran"
          '';
        };
        other = pkgs.mkShell { shellHook = "export LSH_HOOK=other"; };
      });
    };
}
