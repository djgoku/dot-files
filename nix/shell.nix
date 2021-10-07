let
  pkgs = import <nixpkgs> {
    overlays = [
      (import (builtins.fetchTarball {
        url = https://github.com/nix-community/emacs-overlay/archive/master.tar.gz;
      }))
    ];
  };

  customPython = pkgs.python39.buildEnv.override {
    extraLibs = [
      pkgs.python39Packages.virtualenv
    ];
  };

in
pkgs.mkShell {
  buildInputs = with pkgs; [
    autoconf
    cmake
    emacsGit
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
