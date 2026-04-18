{
  description = "Basic Haskell flake";
  inputs.haskell-nix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskell-nix/nixpkgs-2511";
  inputs.nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.hls = { url = "github:haskell/haskell-language-server/fourmolu-ghc-9.14"; flake = false; };
  inputs.simple-http-server = { url = "github:TheWaWaR/simple-http-server/e79ddd3cd12db97062b4a33adc2e436d0022f4be"; flake = false; };
  inputs.browser-wasi-shim = { url = "https://registry.npmjs.org/@bjorn3/browser_wasi_shim/-/browser_wasi_shim-0.3.0.tgz"; flake = false; };
  inputs.ws = { url = "https://registry.npmjs.org/ws/-/ws-8.18.0.tgz"; flake = false; };
  inputs.reflex-dom = { url = "github:ymeister/reflex-dom/ghc(js)-9.10"; flake = false; };
  outputs = inputs@{ self, nixpkgs, nixpkgs-unstable, flake-utils, haskell-nix, ... }:
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
                    ([
                      p.wasi32
                    ] ++ final.lib.optionals final.stdenv.hostPlatform.isLinux
                      [
                        # p.musl64
                        # p.aarch64-multiplatform
                      ]
                    );
                shell.nativeBuildInputs =
                  [
                    pkgs.simple-http-server
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
                        forced-wasm-ghc-pkg = pkgs.writeShellScriptBin "ghc-pkg" ''
                          exec wasm32-unknown-wasi-ghc-pkg "$@"
                        '';
                      in
                      pkgs.writeShellScriptBin "wasm32-unknown-wasi-cabal" ''
                        PATH="${forced-wasm-ghc-pkg}/bin:$PATH" \
                        LD_LIBRARY_PATH="${wasm-dummy-liblibdl}/lib''${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}" \
                        NIX_LDFLAGS=$(echo "$NIX_LDFLAGS" | tr ' ' '\n' | grep -v 'libffi-[0-9]' | tr '\n' ' ') \
                        NIX_LDFLAGS_FOR_TARGET=$(echo "$NIX_LDFLAGS_FOR_TARGET" | tr ' ' '\n' | grep -v 'libffi-[0-9]' | tr '\n' ' ') \
                        exec cabal \
                          --with-ghc=wasm32-unknown-wasi-ghc \
                          --with-compiler=wasm32-unknown-wasi-ghc \
                          --with-ghc-pkg=wasm32-unknown-wasi-ghc-pkg \
                          --with-hsc2hs=wasm32-unknown-wasi-hsc2hs \
                          $(builtin type -P "wasm32-unknown-wasi-pkg-config" &> /dev/null && echo "--with-pkg-config=wasm32-unknown-wasi-pkg-config") \
                          "$@"
                      ''
                    )
                  ];
                shell.tools.cabal = "latest";
                shell.tools.haskell-language-server = {
                  src = inputs.hls;
                  cabalProjectLocal = with pkgs.lib;
                    concatStringsSep "\n"
                      (builtins.filter (line: !(hasPrefix "import" line))
                        (splitString "\n" (builtins.readFile "${inputs.hls}/cabal-ghc914.project"))
                      ) + "  --sha256: 0ljp9g0kq1skwdf1d8hisnwr8nmls03ailv5zb8mk6whdap6nala"
                  ;
                };
                modules = [{
                  packages.miso.patches = [
                    # https://github.com/dmjio/miso/pull/1486
                    # incompatibility with `foreign-store`, fix merged but not in Miso 1.9
                    (pkgs.fetchpatch {
                      url = "https://github.com/dmjio/miso/commit/ec47c61.diff";
                      hash = "sha256-BT58B1PH8BihMkdEpyFOI2UG/iPmSGKo0LLu+xNM1Gs=";
                    })
                  ];
                  packages.reflex-dom.flags = {
                    use-warp = !final.stdenv.hostPlatform.isWasm;
                    webkit2gtk = false;
                  };
                }];
                cabalProjectLocal = ''
source-repository-package
  type: git
  location: https://github.com/ymeister/reflex-dom
  tag: ${inputs.reflex-dom.rev}
  subdir: reflex-dom reflex-dom-core
                '';
                inputMap = {
                  "https://github.com/ymeister/reflex-dom" = inputs.reflex-dom;
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
          # https://github.com/TheWaWaR/simple-http-server/issues/11#issuecomment-4075592693
          (final: prev: with (import nixpkgs-unstable { inherit system; }); {
            simple-http-server = callPackage "${nixpkgs-unstable}/pkgs/by-name/si/simple-http-server/package.nix" {
              rustPlatform = rustPlatform // {
                buildRustPackage = args: rustPlatform.buildRustPackage (finalAttrs: args finalAttrs // {
                  version = "0.8.0";
                  src = inputs.simple-http-server;
                  cargoHash = "sha256-Ji43cp/+fEJ+z0mTIS/CnId1JP9xk9Ti0CwRRKY2saE=";
                  buildFeatures = [ "tls" ];
                });
              };
            };
          })
        ];
        pkgs = import nixpkgs { inherit system overlays; inherit (haskell-nix) config; };
      in
      pkgs.myHaskellProject.flake { });
}
