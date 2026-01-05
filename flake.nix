{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=25.11";
  };

  outputs =
    {
      nixpkgs,
      ...
    }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs { system = system; };
      libs = with pkgs; [
        ocaml
        dune_2
        ocamlPackages.findlib
        ocamlformat

        # GFX_SDL module's libraries
        ocamlPackages.tsdl
        ocamlPackages.tsdl-image
        ocamlPackages.tsdl-ttf

        # GFX_JSOO module's libraries
        ocamlPackages.js_of_ocaml
        ocamlPackages.js_of_ocaml-ppx
      ];
      ppfa = pkgs.ocamlPackages.buildDunePackage {
        pname = "ppfa";
        version = "1.0.0";

        minimalOcamlVersion = "5.2.1";

        src = ./.;
      };
    in
    {
      formatter.${system} = nixpkgs.legacyPackages.${system}.nixfmt-tree;
      devShells.${system}.default = pkgs.mkShell {
        packages = libs;
      };

      packages.${system}.default = ppfa;
    };
}
