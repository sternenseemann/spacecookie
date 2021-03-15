{ mkDerivation, aeson, async, attoparsec, base, bytestring
, containers, directory, download-curl, fast-logger
, filepath-bytestring, hxt-unicode, lib, mtl, process, socket
, systemd, tasty, tasty-expected-failure, tasty-hunit, text
, transformers, unix
}:
mkDerivation {
  pname = "spacecookie";
  version = "0.2.1.2";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    attoparsec base bytestring containers directory filepath-bytestring
    hxt-unicode mtl socket text transformers unix
  ];
  executableHaskellDepends = [
    aeson attoparsec base bytestring containers directory fast-logger
    filepath-bytestring mtl socket systemd text transformers unix
  ];
  testHaskellDepends = [
    async attoparsec base bytestring containers directory download-curl
    filepath-bytestring process tasty tasty-expected-failure
    tasty-hunit
  ];
  homepage = "https://github.com/sternenseemann/spacecookie";
  description = "Gopher Server Library and Daemon";
  license = lib.licenses.gpl3;
}
