# nix-shell --argstr emacsconfig ~/.config/emacs/init.el

{ emacsconfig ? ~/.config/emacs/init.el,
  commit ? "89f2e82fec9f7c2dde0381976266a245f0072217" }:

let
  pkgs = import <nixpkgs> {
    config = {
      allowUnfree = true;
    };
    overlays = [
      (import (builtins.fetchTarball {
        url = "https://github.com/nix-community/emacs-overlay/archive/${commit}.tar.gz";
      }))
    ];
  };

  customPython = pkgs.python39.buildEnv.override {
    extraLibs = [
      pkgs.python39Packages.virtualenv
    ];
  };

  emacsForCI =
    if pkgs.stdenv.isAarch64 then
      pkgs.emacsWithPackagesFromPackageRequires
        {
          packageElisp = builtins.readFile emacsconfig;
          package = pkgs.emacsGit;
          extraEmacsPackages = epkgs: [
            epkgs.vterm
          ];
        }
    else pkgs.emacsGit;
in
pkgs.mkShell {
  buildInputs = with pkgs; [
    autoconf
    cmake
    emacsForCI
    erlang_odbc_javac
    gcc
    gnupg
    jdk11
    libxml2
    libgccjit
    libtool
    openssl
    openssl.dev
    python39
    customPython
    readline
    sqlite
    tmux
    wxmac
    xz
    zlib
  ] ++ (pkgs.lib.optionals pkgs.stdenv.isDarwin [
    pkgs.darwin.apple_sdk.frameworks.AGL
    pkgs.darwin.apple_sdk.frameworks.Carbon
    pkgs.darwin.apple_sdk.frameworks.Cocoa
  ]);
}
