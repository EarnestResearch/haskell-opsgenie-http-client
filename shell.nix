{ pkgs ? import ./nix/nixpkgs.nix {}
}:

let
  sources = import ./nix/sources.nix;
  node-nix = import ./nix/node.nix {
    inherit pkgs;
    nodejs = pkgs.nodejs;
  };
  niv = import sources.niv {};
in
node-nix.shell.overrideAttrs(old: {
  buildInputs = old.buildInputs ++ [
    pkgs.gnumake
    pkgs.nodePackages.node2nix
    pkgs.nodejs
    pkgs.swagger-codegen
    niv.niv
    node-nix.package
    sources.opsgenie-oas
  ];
  OPSGENIE_OAS = sources.opsgenie-oas;
})
