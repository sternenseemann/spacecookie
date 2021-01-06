let pkgs = import <nixpkgs> {};
    profiled = pkgs.haskellPackages.override {
      overrides = self: super: {
        mkDerivation = args: super.mkDerivation (args // {
          enableLibraryProfiling = true;
        });

        spacecookie = pkgs.haskell.lib.overrideSrc
          (self.callPackage ./spacecookie.nix { }) {
            version = "unstable";
            src = builtins.path {
              name = "spacecookie-source";
              path = ./.;
              filter = pkgs.nix-gitignore.gitignoreFilter "" ./.gitignore;
            };
          };
      };
    };
in

if !pkgs.lib.inNixShell
then profiled.spacecookie
else profiled.spacecookie.env.overrideAttrs (old: {
  nativeBuildInputs = old.nativeBuildInputs ++ [ profiled.policeman ];
})
