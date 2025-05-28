{ depotSrc ? builtins.fetchGit {
    url = "https://code.tvl.fyi";
    ref = "canon";
    rev = "1c7083dafc6e6572803179e51bfb8b1c45e7ca6b";
  }
}:

let
  depot = import depotSrc { };
in

depot.users.sterni.htmlman {
  title = "spacecookie";
  description = ''
    * [Source (GitHub)](https://github.com/sternenseemann/spacecookie)
    * [Source (Mirror)](https://code.sterni.lv/spacecookie)

    spacecookie is a gopher server daemon and library written in Haskell.

    Below you can find the user's documentation in the form of a few man pages.
    A more general overview of the software and installation instructions can be
    found in the
    [README](https://github.com/sternenseemann/spacecookie/blob/master/README.md).

    The developer's documentation for the bundled library is
    [located on Hackage](https://hackage.haskell.org/package/spacecookie).
  '';
  manDir = ./man;
  pages = [
    { name = "spacecookie";           section = 1; }
    { name = "spacecookie.json";      section = 5; }
    { name = "spacecookie.gophermap"; section = 5; }
  ];
  linkXr = "inManDir";
}
