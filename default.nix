{ pkgs ? import <nixpkgs> {} }:

let
  hl = pkgs.haskell.lib;

  src = builtins.path {
    name = "spacecookie-source";
    path = ./.;
    filter = pkgs.nix-gitignore.gitignoreFilter
      (builtins.readFile ./.gitignore) ./.;
  };

  profiled = pkgs.haskellPackages.override {
    overrides = self: super: {
      mkDerivation = args: super.mkDerivation (args // {
        enableLibraryProfiling = true;
      });

      spacecookie = hl.overrideCabal
        (self.callPackage ./spacecookie.nix {})
        (drv: {
          version = "unstable";
          # build from sdist to make sure it isn't missing anything
          src = self.cabalSdist {
            src = ./.;
            name = "spacecookie-unstable-sdist.tar.gz";
          };
          # run integration test
          preCheck = ''
            export SPACECOOKIE_TEST_BIN=./dist/build/spacecookie/spacecookie
          '';
          # install man pages
          postInstall = ''
            install -Dm644 docs/man/*.1 -t "$out/share/man/man1"
            install -Dm644 docs/man/*.5 -t "$out/share/man/man5"
          '';
        });
    };
  };

in

if !pkgs.lib.inNixShell
then profiled.spacecookie
else profiled.spacecookie.env.overrideAttrs (old: {
  nativeBuildInputs = old.nativeBuildInputs ++ [ profiled.policeman ];
})
