{ mkDerivation, aeson, async, attoparsec, base, bytestring
, containers, directory, download-curl, fast-logger
, filepath-bytestring, hxt-unicode, lib, mtl, process, socket
, systemd, tasty, tasty-expected-failure, tasty-hunit, text
, transformers, unix
}:
mkDerivation {
  pname = "spacecookie";
  version = "1.0.0.3";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    async attoparsec base bytestring containers directory
    filepath-bytestring hxt-unicode mtl socket text transformers unix
  ];
  executableHaskellDepends = [
    aeson attoparsec base bytestring containers directory fast-logger
    filepath-bytestring mtl socket systemd text transformers unix
  ];
  testHaskellDepends = [
    attoparsec base bytestring containers directory download-curl
    filepath-bytestring process tasty tasty-expected-failure
    tasty-hunit
  ];
  homepage = "https://github.com/sternenseemann/spacecookie";
  description = "Gopher server library and daemon";
  license = lib.licenses.gpl3Only;
  mainProgram = "spacecookie";
}
