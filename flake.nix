{
  description = "Jambhalarch: Plutarch Development Template";

  nixConfig = {
    extra-experimental-features = [ "nix-command" "flakes" "ca-derivations" ];
    extra-substituters = [ "https://cache.iog.io" "https://cache.zw3rk.com" ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "loony-tools:pr9m4BkM/5/eSTZlkQyRt57Jz7OMBxNSUiMC4FkcNfk="
    ];
    allow-import-from-derivation = "true";
    max-jobs = "auto";
    auto-optimise-store = "true";
  };

  inputs = {
    nixpkgs.follows = "liqwid-nix/nixpkgs";
    nixpkgs-latest.url = "github:NixOS/nixpkgs";
    liqwid-nix = {
      url = "github:iburzynski/liqwid-nix";
      inputs.nixpkgs-latest.follows = "nixpkgs-latest";
    };
    liqwid-libs.url =
      "github:Liqwid-Labs/liqwid-libs";
    plutus-simple-model.url = "github:iburzynski/plutus-simple-model";
    cardano-base.url = "github:input-output-hk/cardano-base/cardano-strict-containers-0.1.1.0";
  };

  outputs = inputs@{ self, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [
        inputs.liqwid-nix.flakeModule
      ];
      systems = [ "x86_64-linux" "aarch64-darwin" "x86_64-darwin" "aarch64-linux" ];
      perSystem = { config, self', inputs', pkgs, system, ... }:
        let
          pkgs = import inputs.nixpkgs-latest { inherit system; };
        in
        {
          # See: https://liqwid-labs.github.io/liqwid-nix/reference/modules.html
          onchain.default = {
            src = ./.;
            ghc.version = "ghc925";
            fourmolu.package = pkgs.haskellPackages.fourmolu;
            hlint = { };
            cabalFmt = { };
            hasktags = { };
            applyRefact = { };
            shell = {
              extraCommandLineTools = with pkgs; [
                bashInteractive
                gnugrep
                httpie
                just
                neovim
                nixpkgs-fmt
                nix-prefetch-git
                nodejs-18_x
                nodePackages.pnpm
                python311
                python311Packages.autopep8
                (vscode-with-extensions.override {
                  vscode = pkgs.vscodium;
                  vscodeExtensions = with pkgs.vscode-extensions; [
                    asvetliakov.vscode-neovim
                    dracula-theme.theme-dracula
                    haskell.haskell
                    jnoortheen.nix-ide
                    justusadam.language-haskell
                    mkhl.direnv
                    ms-python.python
                    ms-python.vscode-pylance
                    esbenp.prettier-vscode
                  ];
                })
                xdg-utils
              ];
            };
            hoogleImage.enable = true;
            enableCabalFormatCheck = true;
            enableHaskellFormatCheck = true;
            enableBuildChecks = true;
            extraHackageDeps = [
              "${inputs.liqwid-libs}/liqwid-plutarch-extra"
              "${inputs.liqwid-libs}/liqwid-script-export"
              "${inputs.liqwid-libs}/plutarch-context-builder"
              "${inputs.liqwid-libs}/plutarch-quickcheck"
              "${inputs.liqwid-libs}/plutarch-unit"
              "${inputs.liqwid-libs.inputs.ply}/ply-core"
              "${inputs.liqwid-libs.inputs.ply}/ply-plutarch"
              "${inputs.plutus-simple-model}/psm"
              "${inputs.plutus-simple-model}/cardano-simple"
              "${inputs.cardano-base}/cardano-strict-containers"
            ];
          };
          ci.required = [ "all_onchain" ];
        };
      flake.hydraJobs.x86_64-linux = (
        self.checks.x86_64-linux
        // self.packages.x86_64-linux

      );
    };
}
