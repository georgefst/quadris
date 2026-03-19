{
  description = "Basic Haskell flake";
  inputs.haskell-nix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskell-nix/nixpkgs-2511";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.hls = { url = "github:haskell/haskell-language-server/fourmolu-ghc-9.14"; flake = false; };
  inputs.browser-wasi-shim = { url = "https://registry.npmjs.org/@bjorn3/browser_wasi_shim/-/browser_wasi_shim-0.3.0.tgz"; flake = false; };
  inputs.ws = { url = "https://registry.npmjs.org/ws/-/ws-8.18.0.tgz"; flake = false; };
  outputs = inputs@{ self, nixpkgs, flake-utils, haskell-nix, ... }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "aarch64-darwin" ] (system:
      let
        overlays = [
          haskell-nix.overlay
          (final: prev: {
            haskell-nix = prev.haskell-nix // {
              compiler = prev.haskell-nix.compiler // {
                ghc9141 = prev.haskell-nix.compiler.ghc9141.override {
                  ghc-patches = prev.haskell-nix.compiler.ghc9141.patches ++
                    (with final.lib; optionals final.stdenv.targetPlatform.isWasm (
                      filter (hasSuffix ".patch") (filesystem.listFilesRecursive ./ghc-wasm-patches))
                    );
                };
              };
            };
          })
          (final: prev: {
            myHaskellProject =
              final.haskell-nix.hix.project {
                src = ./.;
                compiler-nix-name = "ghc9141";
                evalSystem = "x86_64-linux";
                crossPlatforms = p:
                  final.lib.optionals final.stdenv.hostPlatform.isx86_64
                    [
                      p.wasi32
                    ];
                shell.nativeBuildInputs =
                  [
                    (
                      let
                        wasm-dummy-liblibdl = pkgs.runCommand "liblibdl"
                          {
                            nativeBuildInputs = [ pkgs.pkgsCross.wasi32.buildPackages.llvmPackages.clang ];
                          }
                          ''
                            mkdir -p $out/lib
                            echo 'void __liblibdl_stub(void) {}' | wasm32-unknown-wasi-cc -shared -x c - -o $out/lib/liblibdl.so 2>/dev/null
                          '';
                        # Cabal 3.16 resolves `ghc-pkg` via PATH for its internal
                        # `ghc-pkg dump --global`, ignoring `--with-ghc-pkg`. Create
                        # a `ghc-pkg` shim that delegates to the wasm one so cabal
                        # finds it first on PATH.
                        ghc-pkg-shim = pkgs.writeShellScriptBin "ghc-pkg" ''
                          exec wasm32-unknown-wasi-ghc-pkg "$@"
                        '';
                        # The wasm RTS's package config doesn't include libffi's
                        # include/lib dirs (unlike native GHC), so the wasm C
                        # compiler can't find ffi.h or -lffi. Provide them
                        # explicitly via GHC options.
                        libffi-wasm = pkgs.pkgsCross.wasi32.libffi;
                      in
                      pkgs.writeShellScriptBin "wasm32-unknown-wasi-cabal" ''
                        LD_LIBRARY_PATH="${wasm-dummy-liblibdl}/lib''${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}" \
                        NIX_LDFLAGS=$(echo "$NIX_LDFLAGS" | tr ' ' '\n' | grep -v 'libffi-[0-9]' | tr '\n' ' ') \
                        NIX_LDFLAGS_FOR_TARGET=$(echo "$NIX_LDFLAGS_FOR_TARGET" | tr ' ' '\n' | grep -v 'libffi-[0-9]' | tr '\n' ' ') \
                        PATH="${ghc-pkg-shim}/bin:$PATH" \
                        exec cabal \
                          --with-ghc=wasm32-unknown-wasi-ghc \
                          --with-compiler=wasm32-unknown-wasi-ghc \
                          --with-ghc-pkg=wasm32-unknown-wasi-ghc-pkg \
                          --with-hsc2hs=wasm32-unknown-wasi-hsc2hs \
                          --ghc-options="-optc-I${libffi-wasm.dev}/include -optl-L${libffi-wasm}/lib" \
                          $(builtin type -P "wasm32-unknown-wasi-pkg-config" &> /dev/null && echo "--with-pkg-config=wasm32-unknown-wasi-pkg-config") \
                          "$@"
                      ''
                    )
                  ];
                shell.tools.cabal = "latest";
                shell.tools.haskell-language-server = {
                  src = inputs.hls;
                  cabalProjectLocal = ''
                    source-repository-package
                      type: git
                      location: https://github.com/fourmolu/fourmolu.git
                      tag: 34dc872445f993f80780f8b37f3a69f8278fe540
                      --sha256: 0ljp9g0kq1skwdf1d8hisnwr8nmls03ailv5zb8mk6whdap6nala
                  '';
                };
                shell.withHoogle = false;
                shell.shellHook =
                  let
                    node_modules = pkgs.linkFarm "node_modules" [{ name = "ws"; path = inputs.ws; }];
                  in
                  ''
                    export BROWSER_WASI_SHIM="${inputs.browser-wasi-shim}"
                    export NODE_PATH="${node_modules}''${NODE_PATH:+:$NODE_PATH}"
                  '';
              };
          })
        ];
        pkgs = import nixpkgs { inherit system overlays; inherit (haskell-nix) config; };
        flake = pkgs.myHaskellProject.flake { };
      in
      flake // {
        devShells = flake.devShells // {
          default =
            let
              haskellNixShell = flake.devShells.default;
              # The haskell.nix shell with crossPlatforms leaks cross-target
              # library paths (e.g. wasm32 compiler-rt's libgcc.a, libffi-wasm)
              # into NIX_LDFLAGS and NIX_CFLAGS_COMPILE via propagatedBuildInputs,
              # breaking native x86_64 linking. The cross toolchains don't need
              # these — they use their own suffixed env vars.
              #
              # We wrap the shell to strip cross-target paths from these vars.
              # This runs as part of the setup hook, so it works even with
              # `nix develop -c`.
              sanitizeEnvHook = pkgs.makeSetupHook { name = "sanitize-cross-env"; } (
                pkgs.writeText "sanitize-cross-env.sh" ''
                  _stripCrossLdFlags() {
                    echo "$1" | tr ' ' '\n' | grep -v 'wasm\|aarch64-unknown\|unknown-linux-musl\|musl-iconv' | tr '\n' ' ' | xargs
                  }
                  _stripCrossCFlags() {
                    local input="$1" result="" skip=0
                    for arg in $input; do
                      if [ "$skip" = 1 ]; then
                        skip=0
                        if echo "$arg" | grep -q 'wasm\|aarch64-unknown\|unknown-linux-musl\|musl-iconv'; then
                          continue
                        fi
                        result="$result -isystem $arg"
                        continue
                      fi
                      if [ "$arg" = "-isystem" ]; then
                        skip=1
                        continue
                      fi
                      result="$result $arg"
                    done
                    echo "$result" | xargs
                  }
                  sanitizeCrossEnv() {
                    for var in NIX_LDFLAGS NIX_LDFLAGS_FOR_TARGET; do
                      if [ -n "''${!var:-}" ]; then
                        export "$var=$(_stripCrossLdFlags "''${!var}")"
                      fi
                    done
                    for var in NIX_CFLAGS_COMPILE NIX_CFLAGS_COMPILE_FOR_TARGET; do
                      if [ -n "''${!var:-}" ]; then
                        export "$var=$(_stripCrossCFlags "''${!var}")"
                      fi
                    done
                  }
                  postHooks+=(sanitizeCrossEnv)
                ''
              );
            in
            pkgs.mkShell {
              inputsFrom = [ haskellNixShell ];
              nativeBuildInputs = [ sanitizeEnvHook ];
              shellHook = haskellNixShell.shellHook or "";
            };
        };
      });
}
