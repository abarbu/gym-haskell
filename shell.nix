args@{ sources ? (import nix/sources.nix {}),
       withCuda ? false
     }:
let
  package = import ./default.nix args;
  pkgs = package.pkgs;
  # mesaX = pkgs.mesa.override { enableOSMesa = true; };
  # https://github.com/siddharthverma314/pyrl/
  # glfw = pkgs.python38Packages.callPackage ./nix/glfw.nix { inherit sources; };
  # mujoco = pkgs.python38Packages.callPackage ./nix/mujoco.nix { inherit sources; };
  # mesa = pkgs.mesa.override { enableOSMesa = true; };
  # mujoco-py = pkgs.python38Packages.callPackage ./nix/mujoco-py.nix {
  #   inherit sources withCuda pkgs mesa mujoco;
  # };
  # gym-super-mario-bros = pkgs.python38Packages.callPackage ./nix/gym-super-mario-bros.nix {
  #   inherit nes-py;
  # };
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
# stdenv.mkDerivation {name = "outer";dontUnpack = true; installPhase = ""; shellHook = "TEST1=1"; buildInputs = [(stdenv.mkDerivation { name = "inner";dontUnpack = true; shellHook = "exit 1"; installPhase = ""; })];}

# pkgs.mujoco-py

package.shellFor {
  tools = {
    cabal = "latest";
    hpack = "latest";
    hlint = "latest";
    ormolu = "latest";
    haskell-language-server = "latest";
  };
  # LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath buildInputs + ":" + pkgs.mujoco + "/bin";
  # MUJOCO_PY_MUJOCO_PATH = pkgs.mujoco;
  exactDeps = true;
  inherit buildInputs;
}
