{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs = { self, nixpkgs }:
    let
      pkgs = import nixpkgs {
        system = "x86_64-linux"; # Replace with your target system if necessary
      };
      oxBuild = pkgs.rustPlatform.buildRustPackage rec {
        pname = "ox";
        version = "v0.1.1";

        src = pkgs.fetchFromGitHub {
          owner = "pagedMov";
          repo = "ox";
          rev = "45ab58de0a116e75debe7430055d02e3a92cca61";
          hash = "sha256-5XwZmsJF/imB8ZSBM9LCrQRRrG5sbjKl6N7MVYIUIck=";
        };

        doCheck = false; # TODO: Find a way to make tests work

        cargoHash = "sha256-zGNlcCF5G1uPQ8/qU04DrZ+gg+YxqS7oumKRaXXZGeQ=";

        nativeBuildInputs = [
          pkgs.openssl
          pkgs.openssl.dev
          pkgs.pkg-config
        ];

        PKG_CONFIG_PATH = "${pkgs.openssl.dev}/lib/pkgconfig";
        passthru = {
          shellPath = "/bin/ox";
        };
      };
    in
    {
      packages.${pkgs.system}.default = oxBuild;

      devShells.default = pkgs.mkShell {
        nativeBuildInputs = [
          pkgs.rust-bin.stable.latest.default
          pkgs.gcc
          pkgs.clang
          pkgs.pkg-config
          pkgs.libgit2
          pkgs.libssh2
          pkgs.libssh2.dev
          pkgs.openssl
          pkgs.openssl.dev
          pkgs.llvm
          pkgs.libclang
          pkgs.pam
        ];

        shellHook = ''
          exec ox
        '';
      };
    };
}
