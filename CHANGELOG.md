# Revision history for spacecookie

## 0.3.0.0 (UNRELEASED)

Consider this one a late christmas present.

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

* The built-in logging is now configurable.
  See [#10](https://github.com/sternenseemann/spacecookie/issues/10) and
  [#20](https://github.com/sternenseemann/spacecookie/pull/20).
  * A custom handler can be installed to filter and format log messages
  * All log messages are now part of a `LogMessage` sum type which describes
    their type and attached data.
  * This is a **breaking change** if you use the `GopherConfig` constructor,
    as it changed to have one more field, namely `cGopherLogConfig`.
  * Logging related API is documented and implemented in `Network.Gopher.Log`.
* Allow specifying an address to listen on.
  See [#13](https://github.com/sternenseemann/spacecookie/issues/13) and
  [#19](https://github.com/sternenseemann/spacecookie/pull/19).
  * `GopherConfig` now has an additional field `cListenAddr` which can be used
    to specify another listening address than `::` to listen on. The given
    address or hostname is resolved using `getaddrinfo`.
  * The config option is ignored if `runGopherManual` is used. The old behaviour
    is used if `cListenAddr` is `Nothing` (which is the case for
    `defaultGopherConfig`).
  * This is a **breaking change** if you use the `GopherConfig` constructor
    directly.
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
