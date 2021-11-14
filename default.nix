let
  sources = import nix/sources.nix {};
  haskell-nix = (import sources."haskell.nix" {});
  nixpkgs = haskell-nix.pkgs;
  gitignore = (import sources."gitignore.nix" {
    inherit (nixpkgs) lib;
  }).gitignoreSource;

  src = nixpkgs.lib.cleanSourceWith {
    name = "gym";
    src = gitignore ./.;
  };
  pkgs = import haskell-nix.sources.nixpkgs
    (haskell-nix.nixpkgsArgs //
     { config.allowUnfree = true;
       overlays =
         haskell-nix.overlays ++
         [(self: super: {
            "python3.8" = pkgs.python38Packages.python;
          })];
     });
in
pkgs.haskell-nix.stackProject {
  inherit src;
  modules = [({pkgs, ...}: {
    packages.cpython.patches = [ ./patches/haskell-cpython-3.5.1-nixify-internals-getsize.patch ];
    packages.gym.components.library.build-tools =
      [ pkgs.python38Packages.python
        pkgs.python38Packages.gym
      ];
    packages.gym.components.tests.gym-test.build-tools =
      [ pkgs.python38Packages.python
        pkgs.python38Packages.gym
      ];
    packages.cypthon.components.library.build-tools =
      [ pkgs.python38Packages.python
        pkgs.python38Packages.gym
      ];
    doHaddock = false;
  })];
}
