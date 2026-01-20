{
  description = "META II compiler-compiler implementation with C VM and Scheme assembler";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, utils }:
    utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
      in
      {
        packages.default = pkgs.stdenv.mkDerivation {
          pname = "meta-ii";
          version = "0.1.0";
          src = ./.;

          buildInputs = [ pkgs.guile ];

          buildPhase = ''
            make all
          '';

          installPhase = ''
            mkdir -p $out/bin $out/share/meta-ii
            cp vm $out/bin/meta-ii-vm
            cp *.scm $out/share/meta-ii/
            cp *.meta $out/share/meta-ii/
            cp *.masm $out/share/meta-ii/
          '';
        };

        devShells.default = pkgs.mkShell {
          buildInputs = [
            pkgs.gcc
            pkgs.gnumake
            pkgs.guile
            pkgs.xxd
          ];

          shellHook = ''
            echo "META II Development Environment"
            echo "Build VM: make all"
            echo "Run VM: ./vm <bytecode-file>"
            echo "Assemble: guile -l assembler.scm"
          '';
        };
      }
    );
}
