{
  description = "BAP (Angs12 fork) as a standalone Nix package";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs = { self, nixpkgs }:
    let
      systems = [ "x86_64-linux" ];
      forAllSystems = f:
        nixpkgs.lib.genAttrs systems
          (system: f nixpkgs.legacyPackages.${system} system);
    in
    {
      packages = forAllSystems (pkgs: _:
        let
          bap = pkgs.callPackage ./nix/package.nix {
            ocamlPackages = pkgs.ocaml-ng.ocamlPackages_5_3;
            llvmPackages = pkgs.llvmPackages_19;
            src = ./.;
          };
        in
        {
          inherit bap;
          default = bap;
        });
    };
}
