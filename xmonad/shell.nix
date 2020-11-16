{ pkgs ? import <nixpkgs> {} }:

let
  haskell = pkgs.haskellPackages.ghcWithPackages (p: [
      p.xmonad
      p.xmonad-contrib
      p.xmonad-extras
      p.haskell-language-server
      p.dbus
    ]);
in

pkgs.mkShell {
  buildInputs = [
    # keep this line if you use bash
    haskell
    pkgs.bashInteractive
  ];
}
