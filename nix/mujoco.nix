{ stdenv
, fetchurl
, autoPatchelfHook
, libGL
, xorg
, writeText
, sources
, mesa
, withCuda ? false
}:
stdenv.mkDerivation rec {
  pname = "mujoco";
  version = "2.1";
  src = sources.mujoco;

  unpackCmd = "tar xf $src";
  # nativeBuildInputs = [ tar ];

  buildInputs = [
    autoPatchelfHook
    stdenv.cc.cc.lib
    libGL
    xorg.libX11
    xorg.libXinerama
    xorg.libXxf86vm
    xorg.libXcursor
    xorg.libXrandr
  ];
  installPhase = ''
    mkdir $out
    # copy required folders
    for folder in bin include model; do
      cp -r $folder $out/$folder
    done
    # make lib folder
    mkdir $out/lib
    ln -s $out/bin/*.so $out/lib/
  '';

  testPhase = ''
    cd sample
    make
  '';

  ldMesaPath = "${stdenv.lib.makeLibraryPath [mesa.osmesa]}";
  ldGLPath = "${stdenv.lib.makeLibraryPath [libGL]}";
  setupHook = writeText "setupHook.sh" ''
    addMujoco () {
        export MUJOCO_PY_MUJOCO_PATH=@out@
        export MUJOCO_BUILD_GPU=${builtins.toString withCuda}
        addToSearchPath LD_LIBRARY_PATH @out@/bin
        addToSearchPath LD_LIBRARY_PATH ${ldMesaPath}
        addToSearchPath LD_LIBRARY_PATH ${ldGLPath}
    }
    addEnvHooks "$hostOffset" addMujoco
    addEnvHooks "$targetOffset" addMujoco
  '';
}
