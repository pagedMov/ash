{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    rust-overlay.url = "github:oxalica/rust-overlay";
    crate2nix.url = "github:nix-community/crate2nix";
  };
  outputs = { self, crate2nix, nixpkgs, flake-utils, rust-overlay }:
    flake-utils.lib.eachDefaultSystem
      (system:
        let
          overlays = [ (import rust-overlay) ];
          pkgs = import nixpkgs {
            inherit system overlays;
          };
          oxBuild = pkgs.rustPlatform.buildRustPackage rec {
            pname = "ox";
            version = "v0.1.0-alpha";

            src = pkgs.fetchFromGitHub {
              owner = "pagedMov";
              repo = "ox";
              rev = "${version}";
              hash = "sha256-Lo3vr/k1o9tPiob/84ZhJ45R/Lew6x+KO0Nu6KEesHA=";
            };

            doCheck = false; # TODO: Find a way to make tests work

            cargoHash = "sha256-j0w8jsP1MNRwR00rGI4Fz24odX7m0N2cyqg1dM8hKW4=";

            nativeBuildInputs = [
              pkgs.openssl
              pkgs.openssl.dev
              pkgs.pkg-config
            ];

            PKG_CONFIG_PATH = "${pkgs.openssl.dev}/lib/pkgconfig";
          };
        in
        with pkgs;
        rec {
          packages = {
            default = oxBuild;
            inherit rust-toolchain;
          };
          devShells.default = mkShell {
            nativeBuildInputs = [
              rust-bin.stable.latest.default
              gcc
              clang
              pkg-config
              libgit2
              libssh2
              libssh2.dev
              openssl
              openssl.dev
              llvm
              libclang
              pam
            ];
            shellHook = ''
              exec ox
            '';
          };
        }
      );
}
