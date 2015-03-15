{ mkDerivation, base, bytestring, containers, directory, filepath
, network, stdenv, unix
}:
mkDerivation {
  pname = "spacecookie";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [
    base bytestring containers directory filepath network unix
  ];
  description = "gopher server daemon";
  license = stdenv.lib.licenses.gpl3;
}
