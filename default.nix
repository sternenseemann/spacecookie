let pkgs = import <nixpkgs> {};
    hl = pkgs.haskell.lib;
    profiled = pkgs.haskellPackages.override {
      overrides = self: super: {
        mkDerivation = args: super.mkDerivation (args // {
          enableLibraryProfiling = true;
        });

        spacecookie = hl.overrideCabal
          (self.callPackage ./spacecookie.nix {})
          (drv: {
            version = "unstable";
            src = builtins.path {
              name = "spacecookie-source";
              path = ./.;
              filter = pkgs.nix-gitignore.gitignoreFilter "" ./.gitignore;
            };
            # run integration test
            preCheck = ''
              export SPACECOOKIE_TEST_BIN=./dist/build/spacecookie/spacecookie
            '';
            postCheck = ''
              cat dist/test/spacecookie-*.log
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
