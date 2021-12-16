args@{ sources ? (import nix/sources.nix {}),
       withCuda ? false
     }:
let
  package = import ./default.nix args;
  pkgs = package.pkgs;
  buildInputs = [ pkgs.python38Packages.python
                  pkgs.python38Packages.gym
                  pkgs.python38Packages.pyglet
                  pkgs.swig
                  #
                  (pkgs.python38Packages.buildPythonPackage rec {
                    pname = "box2d-py";
                    version = "2.3.8";
                    src = pkgs.python38Packages.fetchPypi {
                      inherit pname version;
                      sha256 = "1dqsj9h2jjvyx4p4lzy2bj44crm8sp9lkzlfahbv76q7asygpb5x";
                    };
                    nativeBuildInputs = [ pkgs.swig ];
                    doCheck = false;
                  })
                  # needed for visualization
                  pkgs.python38Packages.scipy
                  pkgs.python38Packages.virtual-display
                  pkgs.python38Packages.xvfbwrapper
                  pkgs.python38Packages.pyopengl
                  pkgs.xvfb-run
                  pkgs.mesa
                  pkgs.mesa.drivers
                  pkgs.mesa.dev
                  pkgs.libGLU
                  pkgs.libGL
                  pkgs.xorg.libXinerama
                  pkgs.xorg.libXcursor
                  # needed for various environments
                  pkgs.box2d
                  pkgs.atari-py
                  pkgs.nes-py
                  pkgs.mujoco
                  pkgs.mujoco-py
                  pkgs.mesa.osmesa
                  pkgs.gym-super-mario-bros
                ];
  stdenv = pkgs.stdenv;
in
package.shellFor {
  tools = {
    cabal = "latest";
    hpack = "latest";
    hlint = "latest";
    ormolu = "latest";
    haskell-language-server = "latest";
  };
  exactDeps = true;
  inherit buildInputs;
}
