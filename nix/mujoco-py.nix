{ withCuda ? false
, fetchFromGitHub
, mesa
, python3
, libGL
, gcc
, stdenv
, callPackage
, autoPatchelfHook
, xorg
, lib
, libglvnd
, imageio
, numpy
, cython
, cffi
, lockfile
, buildPythonPackage
, sources
, fasteners
, mujoco
, pkgs
}:
let
  src = sources.mujoco-py;
in
buildPythonPackage {
  inherit src;
  pname = "mujoco-py";
  version = "2.0.2.5";
  requirements = builtins.readFile "${src}/requirements.txt";

  python = python3;
  MUJOCO_BUILD_GPU = withCuda;
  nativeBuildInputs = [
    autoPatchelfHook
  ];
  propagatedBuildInputs = [
    imageio
    numpy
    cython
    (callPackage ./glfw.nix { inherit sources; })
    cffi
    lockfile
    fasteners
  ];
  buildInputs = [
    mesa
    mesa.osmesa
    mujoco
    python3
    libGL
    gcc
    stdenv.cc.cc.lib
  ] ++ lib.optionals withCuda [ xorg.libX11 libglvnd ];

  # hacks to make the package work
  postInstall = lib.optionalString withCuda ''
    patchelf --add-needed libEGL.so $out/lib/${python3.libPrefix}/site-packages/mujoco_py/cymj.cpython*.so
    patchelf --add-needed libOpenGL.so $out/lib/${python3.libPrefix}/site-packages/mujoco_py/cymj.cpython*.so
  '';

  doCheck = false;

  patches = [ ../patches/mujoco-py-2.0.2.5-no-lockfile.patch ];

  LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath [ mujoco mesa mesa.osmesa libGL ] + ":" + mujoco + "/bin";
  MUJOCO_PY_MUJOCO_PATH = mujoco;
}
