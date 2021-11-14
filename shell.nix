let
  package = (import ./default.nix);
  pkgs = package.pkgs;
  commit = "73de0e330fd611769c192e98c11afc2d846d822b";  # from: Mon Apr 27 2020
  atari-py = (with pkgs; with pkgs.python38Packages;
    # https://github.com/nixvital/ml-pkgs/blob/bbc71d9edfc577704efaf795ee522963da7d2fd5/pkgs/atari-py-with-rom/default.nix
    (let atari-roms = builtins.fetchurl {
      url = "http://www.atarimania.com/roms/Roms.rar";
      sha256 = "1654mhnsimb79qb99im6ka2i758b6r43m38gz04d19rxpngqfdaf";
    };

    # The following script is used to install ROMS in to atari-py.
    #
    # Inside atari-py there is a md5.txt with all the md5 hashes of
    # the roms (.bin files) that needs to be installed.
    #
    # This is done by downloading the ROMs in atari-roms (see above)
    # and run this script.
    import-atari-roms = writeShellScriptBin "import-atari-roms" ''
      MD5_FILE=$2
      ROM_DIRECTORY=$3
      TARGET_DIRECTORY=$4
      
      # Step 1 - Construct the hash -> bin file map
      
      declare -A bin_hash_map
      
      if [ ! -e ''${MD5_FILE} ]; then
          echo "[ERROR] ''${MD5_FILE} does not exist!"
          exit 125
      fi
      
      if [ ! -d ''${ROM_DIRECTORY} ]; then
          echo "[ERROR] ''${ROM_DIRECTORY} does not exist!"
          exit 125
      fi
      
      echo "Constructing .bin file and hash mapping from ''${MD5_FILE}"
      
      let entry_count=0
      
      while IFS=" " read -r hash fname; do
          if [ ''${#hash} == 32 ]; then
              bin_hash_map[''${hash}]=''${fname}
              let entry_count++
          fi
      done < ''${MD5_FILE}
      
      echo "Finished reading the md5 list, found ''${entry_count} entries."
      # Step 2 - Copy the matched bin files
      ORIGINAL_IFS="$IFS"
      IFS=$'\n' # Temporarily override IFS to be the newline
      for f in $(find ''${ROM_DIRECTORY} -type f -name "*.bin"); do
          md5=$(md5sum "$f" | cut -c 1-32)
          bin_file=''${bin_hash_map[$md5]-NOMATCH}
          if [ ''${bin_file} != "NOMATCH" ]; then
              dest="''${TARGET_DIRECTORY}/''${bin_file}"
              cp "$f" "$dest"
              echo "Copied matched file ($f) to ($dest)"
          fi
      done
      IFS="$ORIGINAL_IFS"
    '';

     in buildPythonPackage rec {
       pname = "atari-py";
       version = "0.2.9";
       format = "wheel";

       src = builtins.fetchurl (import
         (builtins.fetchurl "https://raw.githubusercontent.com/nixvital/ml-pkgs/main/pkgs/atari-py-with-rom/wheel-urls.nix")
         { inherit version isPy37 isPy38 isPy39; });

       disabled = !(isPy37 || isPy38 || isPy39);

       propagatedBuildInputs = [ numpy six ];

       buildInputs = [
         stdenv.cc.cc.lib
       ];
       
       nativeBuildInputs = [
         autoPatchelfHook
       ];
       
       postFixup = let
         pythonName = if isPy37 then "python3.7" else if isPy38 then "python3.8" else "python3.9";
         pkgPath = "$out/lib/${pythonName}/site-packages/atari_py";
       in ''
    pushd ${pkgPath}
    mkdir roms_temp
    ${unrar}/bin/unrar x "${atari-roms}" roms_temp/
    pushd roms_temp    
    ${unzip}/bin/unzip "HC ROMS.zip"
    ${unzip}/bin/unzip "ROMS.zip"
    popd
    head -n 10 ${pkgPath}/ale_interface/md5.txt
    ${import-atari-roms}/bin/import-atari-roms ${pkgPath} \
        ${pkgPath}/ale_interface/md5.txt \
        ${pkgPath}/roms_temp \
        ${pkgPath}/atari_roms
    rm -rf roms_temp/
    popd
  '';

       pythonImportsCheck = [ "atari_py" ];  
     }));
  buildInputs = [ pkgs.python38Packages.python
                  pkgs.python38Packages.gym
                  pkgs.python38Packages.pyglet
                  pkgs.box2d
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
                  atari-py
                  #
                  pkgs.python38Packages.scipy
                  pkgs.python38Packages.virtual-display
                  pkgs.python38Packages.xvfbwrapper
                  pkgs.python38Packages.pyopengl
                  pkgs.xvfb-run
                  pkgs.mesa
                  pkgs.mesa.drivers
                  pkgs.mesa.dev
                  pkgs.mesa-demos
                  pkgs.libGLU
                  pkgs.libGL
                  pkgs.xorg.libXinerama
                  pkgs.xorg.libXcursor
                  pkgs.glxinfo
                  pkgs.freeglut
                  pkgs.virtualgl
                  pkgs.xorg.xkbcomp
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
  LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath buildInputs;
  exactDeps = true;
  inherit buildInputs;
}
