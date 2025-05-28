{ mkDerivation, aeson, async, attoparsec, base, bytestring
, containers, directory, download-curl, fast-logger
, filepath-bytestring, lib, mtl, process, socket, systemd, tasty
, tasty-expected-failure, tasty-hunit, text, transformers, unix
, utf8-string
}:
mkDerivation {
  pname = "spacecookie";
  version = "1.0.0.3";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    async attoparsec base bytestring containers directory
    filepath-bytestring mtl socket text transformers unix utf8-string
  ];
  executableHaskellDepends = [
    aeson attoparsec base bytestring containers directory fast-logger
    filepath-bytestring mtl socket systemd text transformers unix
    utf8-string
  ];
  testHaskellDepends = [
    attoparsec base bytestring containers directory download-curl
    filepath-bytestring process tasty tasty-expected-failure
    tasty-hunit utf8-string
  ];
  homepage = "https://github.com/sternenseemann/spacecookie";
  description = "Gopher server library and daemon";
  license = lib.licenses.gpl3Only;
  mainProgram = "spacecookie";
}
