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

## Configuration

In order to run your new gopher server, you have to configure it first.
You can find an example configuration file in `./etc/spacecookie.json`.
JSON is mostly used due to legacy reasons, but has the plus of being
convenient to generate.

The config file should consist of a single object which may contain the
following fields (some are required). An example can be found in
[`etc/spacecookie.json](./etc/spacecookie.json).

### `hostname`

The public hostname spacecookie will be reachable through. This hostname is
used by gopher clients to determine the target address when following entries
in a gopher menu.

### `user`

The user that should be used to run spacecookie. If given, spacecookie will switch
to this user after setting up its socket. This requires the possibility to call
`setuid` and `setgid`.

Can be omitted or set to `null`, if root privileges are not needed (e. g. if
systemd socket activation or a non well-known port is used).

### `root`

Directory which contains the files to serve via gopher.

### `listen`

Describes the address to listen on. Its value must be
an object of the following form.

```json
{
  "addr": "::1",
  "port": 70
}
```

* The `addr` field describes the address to listen on and can be
  anything that can be resolved by `getaddrinfo`, e. g. a hostname
  or an IP address. It may be omitted and defaults to `::`.
* The `port` field describes the port to listen on. If it is omitted
  the well-known port for gopher is used (70).

`listen` may be omitted altogether in which case the default values
described before are used.

### `log`

Configure log output. The option expects an object of the following
format:

```json
{
  "enable": true,
  "hide-ips": true,
  "hide-time": false,
  "level": "info"
}
```

* The `enable` field determines whether to enable logging. Defaults to
  `true`.
* The `hide-ips` field determines whether IP addresses should show up in
  the log. This is an optional setting which defaults to `true` in order
  to avoid leaking user related data to the log output.
* The `hide-time` field can be set to `true` to disable timestamps in the
  log output, e. g. if you are using systemd. Defaults to `false`.
* The `level` field can either be set to `"info"` or `"error"`. Default
  value is `"info"`.

If `log` is not given, it defaults to enabled with the default values for
all optional settings.

### `port`

Legacy option for backwards compatibility, use `listen` → `port` instead.

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

So what does that all mean? These are the rules for a gophermap file:

* Info lines (sometimes also called comment lines) are just lines of text
  without any tab characters. They will be displayed as lines of text by
  the gopher client.
* Menu entries for files or directories start with a single character which
  specifies the file type, followed by the text for that file without a space
  or tab between them! Then the path is added after a tab.
* Links to other servers are like file/directory menu entries but the server's
  hostname and its port must be added (tab-separated).

Further documentation:

* Detailed documentation: `spacecookie-gophermap(5)`
* [File type characters](https://tools.ietf.org/html/rfc1436#page-10)
* [Original gophermap description from Bucktooth](./docs/bucktooth-gophermap.txt)

## Portability

spacecookie's portability is mostly limited by
[haskell-socket](https://github.com/lpeterse/haskell-socket)
which should work on any POSIX-compliant Operating system.

However I personally have only tested spacecookie on GNU/Linux
so far. Feel free to send me an email or open an issue if you
have any trouble!

Windows is currently not supported as some Unix-specific features
are used, but there is probably little demand for it as well.
