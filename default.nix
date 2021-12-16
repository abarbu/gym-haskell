{ sources ? (import nix/sources.nix {}),
  haskell-nix ? (import sources."haskell.nix" {}),
  nixpkgs ? haskell-nix.pkgs,
  gitignore ? ((import sources."gitignore.nix" {
    inherit (nixpkgs) lib;
  }).gitignoreSource),
  withCuda ? false
}:

let
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
           "python3.8" = super.python38Packages.python;
           mesa = super.mesa.override {
             enableOSMesa = true;
           };
           atari-py = import ./nix/atari-py.nix { pkgs = self; };
           nes-py = import ./nix/nes-py.nix { pkgs = self; };
           # mesa = pkgs.mesa.override { enableOSMesa = true; };
           mujoco = super.python38Packages.callPackage ./nix/mujoco.nix {
             inherit sources;
             mesa = self.mesa;
           };
           mujoco-py = super.python38Packages.callPackage ./nix/mujoco-py.nix {
             inherit sources withCuda;
             pkgs = self;
             mesa = self.mesa;
             mujoco = self.mujoco;
           };
           gym-super-mario-bros = super.python38Packages.callPackage ./nix/gym-super-mario-bros.nix {
             nes-py = self.nes-py;
           };
           box2d-py = (pkgs.python38Packages.buildPythonPackage rec {
             pname = "box2d-py";
             version = "2.3.8";
             src = pkgs.python38Packages.fetchPypi {
               inherit pname version;
               sha256 = "1dqsj9h2jjvyx4p4lzy2bj44crm8sp9lkzlfahbv76q7asygpb5x";
             };
             nativeBuildInputs = [ pkgs.swig ];
             doCheck = false;
           });
         })];
     });
  dependencies = [ pkgs.python38Packages.python
                   pkgs.python38Packages.gym
                   pkgs.atari-py
                   pkgs.nes-py
                   pkgs.gym-super-mario-bros
                   pkgs.box2d
                   pkgs.box2d-py
                   pkgs.mujoco-py
                   pkgs.mujoco
                 ];
in
pkgs.haskell-nix.stackProject {
  inherit src;
  modules = [({pkgs, ...}: {
    packages.gym.components.library.build-tools = dependencies;
    packages.gym.components.tests.gym-test.build-tools = dependencies;
    packages.cypthon.components.library.build-tools = dependencies;
    doHaddock = false;
  })];
}
