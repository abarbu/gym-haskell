args@{ sources ? (import nix/sources.nix {}),
       withCuda ? false
     }:
let
  package = import ./default.nix args;
  pkgs = package.pkgs;
  atari-py = import ./nix/atari-py.nix {inherit pkgs;};
  nes-py = import ./nix/nes-py.nix {inherit pkgs;};
  # mesaX = pkgs.mesa.override { enableOSMesa = true; };
  # https://github.com/siddharthverma314/pyrl/
  # glfw = pkgs.python38Packages.callPackage ./nix/glfw.nix { inherit sources; };
  mujoco = pkgs.python38Packages.callPackage ./nix/mujoco.nix { inherit sources; };
  mesa = pkgs.mesa.override { enableOSMesa = true; };
  mujoco-py = pkgs.python38Packages.callPackage ./nix/mujoco-py.nix {
    inherit sources withCuda pkgs mesa mujoco;
  };
  gym-super-mario-bros = pkgs.python38Packages.callPackage ./nix/gym-super-mario-bros.nix {
    inherit nes-py;
  };
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
                  mesa
                  mesa.drivers
                  mesa.dev
                  mesa.osmesa
                  pkgs.libGLU
                  pkgs.libGL
                  pkgs.xorg.libXinerama
                  pkgs.xorg.libXcursor
                  # needed for various environments
                  pkgs.box2d
                  atari-py
                  nes-py
                  mujoco
                  mujoco-py
                  gym-super-mario-bros
                ];
in
package.shellFor {
  tools = {
    cabal = "latest";
    hpack = "latest";
    hlint = "latest";
    ormolu = "latest";
    haskell-language-server = "latest";
  };
  LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath buildInputs + ":" + mujoco + "/bin";
  MUJOCO_PY_MUJOCO_PATH = mujoco;
  exactDeps = true;
  inherit buildInputs;
}
