# nix-shell --argstr emacsconfig ~/.config/emacs/init.el

{ emacsconfig ? ~/.config/emacs/init.el
, commit ? "5b3d95676be5c6963c4d16f21ecf82beb14c6c05" # pragma: allowlist secret
}:

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

  customPython = pkgs.python310.buildEnv.override {
    extraLibs = [
      pkgs.python310Packages.virtualenv
      pkgs.python310Packages.ipython
      pkgs.python310Packages.requests
      pkgs.python310Packages.pudb
      pkgs.python310Packages.ipykernel
      pkgs.python310Packages.python-lsp-server
    ];
  };

  emacsForCI =
    pkgs.emacsWithPackagesFromPackageRequires
      {
        packageElisp = builtins.readFile emacsconfig;
        package = pkgs.emacsGit;
        extraEmacsPackages = epkgs: [
          epkgs.vterm
        ];
      };

  # sha256 can be calculated via nix-prefetch-url --unpack http://url
  vscodeExtensions = (with pkgs.vscode-extensions;
    [
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
      sha256 = "1qz8jxpzanaccd5v68z4v1344kw0iy671ksi1bmpyavinlxdkmr8"; # pragma: allowlist secret
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
pkgs.mkShell
{
  buildInputs = with pkgs; [
    _1password
    autoconf
    automake
    awscli2
    beam.packages.erlangR25.elixir_1_14
    cargo
    chromedriver
    cmake
    colima
    curl
    customPython
    direnv
    earthly
    elixir_ls
    emacsForCI
    fd
    gcc
    git
    gnupg
    graphviz
    isync
    jdk11
    jq
    kubectl
    libgccjit
    libiconv
    libxml2
    mailutils
    msmtp
    ngrok
    nixpkgs-fmt
    nodePackages_latest.tailwindcss
    nodejs-14_x
    notmuch
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
    sops
    sqlite
    squashfsTools
    steampipe
    terraform
    terrascan
    texlive.combined.scheme-full # pdf2latex
    tfsec
    tmux
    tree-sitter
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
