# spacecookie

Haskell gopher server daemon and library.

## Features

* implements RFC1436
* optionally supports common protocol extensions:
  * informational entries via the `i`-type
  * [`h`-type and URL entries](http://gopher.quux.org:70/Archives/Mailing%20Lists/gopher/gopher.2002-02%7C/MBOX-MESSAGE/34)
* supports gophermaps (see [below](#adding-content))
* supports systemd socket activation
* provides a library for custom gopher applications ([see documentation](http://hackage.haskell.org/package/spacecookie/docs/Network-Gopher.html))

## Non-Features

spacecookie intentionally does not support:

* HTTP, Gemini: Multi protocol support is a non-goal for spacecookie.
  For HTTP you can [proxy](https://github.com/sternenseemann/gopher-proxy)
  pretty easily, however.
* Search: Gopher supports search transactions, but the spacecookie daemon doesn't offer
  the possibility to add a search engine to a gopherspace. It is however
  entirely possible to implement an index search server using [the
  spacecookie library](https://hackage.haskell.org/package/spacecookie/docs/Network-Gopher.html)

## Installation

* Nix(OS): [`pkgs.haskellPackages.spacecookie`](https://search.nixos.org/packages?channel=unstable&from=0&size=50&sort=relevance&query=spacecookie)
  (see also [below](#on-nixos))
* Cabal: `cabal v2-install spacecookie`
  (see also [hackage package](http://hackage.haskell.org/package/spacecookie))

## Documentation

* User Documentation: [spacecookie(1)](https://sternenseemann.github.io/spacecookie/spacecookie.1.html)
* [Developer Documentation](https://hackage.haskell.org/package/spacecookie)

## Configuration

spacecookie is configured via a JSON configuration file.
All available options are documented in
[spacecookie.json(5)](https://sternenseemann.github.io/spacecookie/spacecookie.json.5.html).
This repository also contains an example configuration file in
[`etc/spacecookie.json`](./etc/spacecookie.json).

## Running

After you've created your config file just start spacecookie like this:

	spacecookie /path/to/spacecookie.json

spacecookie runs as a simple process and doesn't fork or write a PID file.
Therefore any supervisor (systemd, daemontools, ...) can be used to run
it as a daemon.

### With systemd

spacecookie supports systemd socket activation. To set it up you'll need
to install `spacecookie.service` and `spacecookie.socket` like so:

	cp ./etc/spacecookie.{service,socket} /etc/systemd/system/
	systemctl daemon-reload
	systemctl enable spacecookie.socket
	systemctl start  spacecookie.socket
	systemctl start  spacecookie.service # optional, started by the socket automatically if needed

Of course make sure that all the used paths are correct!

How the systemd integration works is explained in
[spacecookie(1)](https://sternenseemann.github.io/spacecookie/spacecookie.1.html#SYSTEMD_INTEGRATION).

### On NixOS

[NixOS](https://nixos.org/nixos/) provides a service module for spacecookie:
[`services.spacecookie`](https://github.com/NixOS/nixpkgs/blob/master/nixos/modules/services/networking/spacecookie.nix).
Setting up spacecookie is as simple as adding the following line to your `configuration.nix`:

	services.spacecookie.enable = true;

For all available options, refer to the NixOS manual:

* [NixOS stable](https://nixos.org/manual/nixos/stable/options.html#opt-services.spacecookie.enable)
* [NixOS unstable](https://nixos.org/manual/nixos/unstable/options.html#opt-services.spacecookie.enable)

## Adding Content

spacecookie acts as a simple file server, only excluding files
or directories that start with a dot. It generates gopher menus
automatically, but you can also use custom ones by adding a
gophermap file.

spacecookie checks for `.gophermap` in every directory it serves and,
if present, uses the menu specified in there.

Such a file looks like this:

	You can just start writing text that
	will be displayed by the gopher client
	without a link to a file. Empty lines are
	also possible.

	1Menu Entry for a directory full of funny stuff	/funny
	IFunny Image	/funny.jpg
	gcat gif	/cat.gif
	0about me	/about.txt
	1Floodgap's gopher server	/	gopher.floodgap.com	70

As you can see, it largely works like the actual gopher menu a server will
send to clients, but allows to omit redundant information and to insert
lines that are purely informational and not associated with a file.
[spacecookie.gophermap(5)](https://sternenseemann.github.io/spacecookie/spacecookie.gophermap.5.html)
explains syntax and semantics in more detail.

The format is compatible with the ones supported by
[Bucktooth](gopher://gopher.floodgap.com/1/buck/) and
[pygopherd](https://github.com/jgoerzen/pygopherd).
If you notice any incompatibilities, please open an issue.

## Portability

spacecookie is regularly tested on GNU/Linux via CI, but
should also work on other Unix-like operating systems.
Most portability problems arise due to
[haskell-socket](https://github.com/lpeterse/haskell-socket)
which is for example known
[not to work on OpenBSD](https://github.com/lpeterse/haskell-socket/issues/63).

Windows support would be possible, but could be tricky as gopher
expects Unix-style directory separators in paths. I personally
don't want to invest time into it, but would accept patches adding
Windows support.
