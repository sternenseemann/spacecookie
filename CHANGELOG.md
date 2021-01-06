# Revision history for spacecookie

## 0.3.0.0 (UNRELEASED)

What a way to start your year.

### Server

* Add new `listen` field to configuration allowing to specify the
  listening address and port. It expects an object with the fields
  `port` and `addr`. The top level `port` option has been *deprecated*
  as a result. It is now possible to bind to the link local address
  `::1` only without listening on public addresses.
  See [#13](https://github.com/sternenseemann/spacecookie/issues/13) and
  [#19](https://github.com/sternenseemann/spacecookie/pull/19).
* Gophermaps now support relative paths like pygopherd does.
  See [#22](https://github.com/sternenseemann/spacecookie/issues/22) and
  [#23](https://github.com/sternenseemann/spacecookie/pull/23).
  * Selectors in gophermaps that start with a `/` are still interpreted
    like before, as well as URLs starting with `URL:`.
  * Selectors to which neither of those conditions apply are treated as
    relative and processed accordingly before sent to gopher clients.
* Fixed parsing of gophermap files whose last line ends in an `EOF`.
* Log output is now configurable via the new `log` field in the
  configuration. Like `listen` it expects an object which supports the
  following fields.
  See [#10](https://github.com/sternenseemann/spacecookie/issues/10) and
  [#20](https://github.com/sternenseemann/spacecookie/pull/20).
  * `enable` allows to enable and disable logging
  * `hide-ips` can be used to hide private information of users from
    log output. This is *now enabled by default*.
  * `hide-time` allows to hide timestamps if your log setup already
    takes care of that.
  * `level` allows to switch between `error` and `info` log level.
* Make `port` and `listen` → `port` settings optional, defaulting to 70.
* GHC RTS options are now enabled by default
* Fix the file not found error message erroneously stating that access of that
  file was not permitted.
* Clarify error message when an URL: selector is sent to spacecookie.
* Print version when `--version` is given
* Print simple usage instructions when `--help` is given or the command line
  can't be parsed.

Log parsing should be backwards compatible. Please open a bug report if
you experience any problems with that or any constellation of the new
settings.

### Library

#### Logging

The built-in logging support has been removed in favor of a log handler the
user can specify in `GopherConfig`. This is a **breaking change** in two ways:

* The type of `GopherConfig` changed as it has a new field called
  `cLogHandler`.
* By default (`defaultGopherConfig`) the spacecookie library no longer
  has logging enabled.

The motivation for this was to enable the library user to influence the log
output more. More specifically the following abilities were to be made
possible for the bundled server daemon:

* It should be possible to hide timestamps in the log output: If you are
  using systemd for example, the journal will take care of those.
* There should be the ability to hide sensitive information from the log
  output: Unless necessary client IP addresses shouldn't be logged to
  disk.
* The log output should be filterable by log level.
* It should be easy for server implementation to also output log messages
  via the same system as the `spacecookie` library.

The best solution to guarantee these properties (and virtually any you could
want) is to let the library user implement logging. This allows any target
output, any kind of logging, any kind of clock interaction to generate
timestamps (or not) etc. This is why the spacecookie library no longer
implements logging. Instead it lets you configure a `GopherLogHandler`
which also can be used by the user application as it is a simple `IO`
action. This additionally scales well: In the simplest case this could
be a trivial wrapper around `putStrLn`.

The second part to the solution is `GopherLogStr` which is the string type
given to the handler. Internally this is currently implemented as a `Seq`
containing chunks of `Builder`s which are coupled with meta data. This
should allow decent performance in building and rendering of `GopherLogStr`s.
The latter of which is relatively convenient using `FromGopherLogStr`.

The tagged chunks are used to allow a clean implementation of hiding sensitive
data: `makeSensitive` can be used to tag all chunks of a `GopherLogStr` which
will then be picked up by `hideSensitive` which replaces all those chunks
with `[redacted]`. This way sensitive information can be contained inline in
strings and users can choose at any given point whether it should remain there
or be hidden.

The new logging mechanism was implemented in
[#29](https://github.com/sternenseemann/spacecookie/pull/29).

Previously it was attempted to make built-in logging more configurable
(see [#13](https://github.com/sternenseemann/spacecookie/issues/13) and
[#19](https://github.com/sternenseemann/spacecookie/pull/19)), but this
was overly complicated and not as flexible as the new solution as well
as more hassle for the library user except in very specific cases.

#### Other changes

* Changes for `Network.Gopher.Util.Gophermap`:
  * Fix the last line being ignored in `parseGophermap` if it ended with an
    `EOF` rather than a newline.
  * Potentially **breaking change**: `parseGophermap` now consumes the end of
    input.
  * **Breaking change**: Support relative paths in gophermaps by wrapping the
    `FilePath` in `GophermapFilePath` signifying if the selector is relative,
    absolute or an URL.
    See [#22](https://github.com/sternenseemann/spacecookie/issues/22) and
    [#23](https://github.com/sternenseemann/spacecookie/pull/23).
    * `GophermapEntry` changed to use `GophermapFilePath` instead of `FilePath`
    * `gophermapToDirectoryResponse` takes an additional parameter describing
      the directory the gophermap is located in to resolve relative to absolute
      selectors.
* `santinizePath` and `santinizeIfNotUrl` have been corrected to `sanitizePath`
  and `sanitizeIfNotUrl` respectively. This is a **breaking change** to the
  interface of `Network.Gopher.Util`.

## 0.2.1.2 Bump fast-logger

* Bump fast-logger dependency, fix build

## 0.2.1.1 Fixed Privilege Dropping

* Server
  * Make `user` parameter in config optional. If it is not given or set to `null`, `spacecookie` won't attempt
    to change its UID and GID. This is especially useful, if socket activation is used. In that case it is not
    necessary to start spacecookie as `root` since systemd sets up the socket, so `spacecookie` can be already
    started by the right user and doesn't need to change UID.
  * Example Systemd config files
    * `SocketMode` is now `660` instead of default `666`.
    * Set `User` and `Group` for `spacecookie.service` as well.
    * Set `"user": null` in `spacecookie.json`
* Library
  * Fixed issue that led to `runGopher*` trying to change UID even if it wasn't possible (not running as root).
    This especially affected the `spacecookie` server, since `cRunUserName` would always be `Just`.
  * Made logging related to `dropPrivileges` clearer.

## 0.2.1.0 Systemd Support

* Improved systemd support.
  * Support for the notify service type
  * Support for socket activation and socket (fd) storage
  * To make use of these new features you'll have to update your service files
* Added `defaultConfig` value to prevent future breakage in software using the
  library when the `GopherConfig` type is extended.
* Pretty print IPv6 addresses in logging

## 0.2.0.1 Hackage release

Fixed a problem hindering the hackage release.

## 0.2.0.0 initial release

* First version. Released on an unsuspecting world. Includes:
  * Library for writing any gopher server / application.
  * File system based gopher server with support for gopher maps.
  * Supports logging, privilege dropping, the gopher protocol and common extensions.
