{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    nixpkgs.follows = "emacs-overlay/nixpkgs";
  };

  outputs = { self, flake-utils, emacs-overlay, nixpkgs }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ emacs-overlay.overlay ];
        };
      in
      rec {
        packages.default = (pkgs.emacs-git-pgtk.override {
          # jww (2025-04-02): https://github.com/NixOS/nixpkgs/issues/395169
          withNativeCompilation = false;
        }).overrideAttrs (old: rec {
          NIX_CFLAGS_COMPILE = (old.env.NIX_CFLAGS_COMPILE or "")
            + pkgs.lib.optionalString pkgs.stdenv.hostPlatform.isDarwin
            " -DFD_SETSIZE=10000 -DDARWIN_UNLIMITED_SELECT";
        });
      }
    );
}
