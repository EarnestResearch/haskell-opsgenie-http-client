{ system ? builtins.currentSystem }:
let
  sources = import ./sources.nix;
  erNix = import sources.er-nix;
in
erNix.pkgsForSystem system
