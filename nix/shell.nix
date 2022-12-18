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

  # sha256 can be calculated via nix-prefetch-url --unpack http://url
  vscodeExtensions = (with pkgs.vscode-extensions; [
  ]) ++ pkgs.vscode-utils.extensionsFromVscodeMarketplace [
    {
      name = "vscode-emacs-friendly";
      publisher = "lfs";
      version = "0.9.0";
      sha256 = "1j4cy77m1077wdl2vvpmzi98y3jkycvf8z84pscs3lkkk1mvcsv1";
    }
    {
      name = "vscode-terminals";
      publisher = "fabiospampinato";
      version = "1.13.0";
      sha256 = "0j96c6486h4073b7551xdr50fir572f22nlkz0y6q52670gdii5y";
    }
    {
      name = "vscode-mjml";
      publisher = "attilabuti";
      version = "1.6.0";
      sha256 = "180rvy17l0x5mg2nqkpfl6bcyqjnf72qknr521fmrkak2dp957yd";
    }
    {
      name = "elixir-ls";
      publisher = "JakeBecker";
      version = "0.9.0";
      sha256 = "1qz8jxpzanaccd5v68z4v1344kw0iy671ksi1bmpyavinlxdkmr8";
    }
    {
      name = "surface";
      publisher = "msaraiva";
      version = "0.7.0";
      sha256 = "1y5m0p4lkr0zfiyshrm9mkg0rzx81zhp6p16mw08jwndvy0396zn";
    }
  ];
  vscode-with-extensions = pkgs.vscode-with-extensions.override {
    vscodeExtensions = vscodeExtensions;
  };
in
pkgs.mkShell {
  buildInputs = with pkgs; [
    _1password
    autoconf
    automake
    beam.packages.erlangR25.elixir_1_14
    chromedriver
    cmake
    curl
    elixir_ls
    emacsForCI
    fd
    gcc
    git
    gnupg
    graphviz
    # inotify-tools
    jdk11
    jq
    libgccjit
    libiconv
    libxml2
    ngrok
    nixpkgs-fmt
    nodejs-14_x
    openssl
    openssl.dev
    pandoc
    postgresql_14
    pre-commit
    readline
    rebar3
    ripgrep
    ruby_3_1
    slack
    sqlite
    squashfsTools
    tmux
    vscode-with-extensions
    wget
    xz
    zlib
  ] ++ (pkgs.lib.optionals pkgs.stdenv.isDarwin [
    pkgs.wxmac
    pkgs.wxGTK31
    pkgs.darwin.apple_sdk.frameworks.AGL
    pkgs.darwin.apple_sdk.frameworks.Carbon
    pkgs.darwin.apple_sdk.frameworks.Cocoa
  ]);
}
