# nix-shell --argstr emacsconfig ~/.config/emacs/init.el

{ emacsconfig ? ~/.config/emacs/init.el
, # 5ee2f08137100840c1db4d017420fc05c440e97e - 2021-10-08
  commit ? "5ee2f08137100840c1db4d017420fc05c440e97e"
}:
let
  pkgs = import <nixpkgs> {
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
