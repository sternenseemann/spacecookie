# Revision history for spacecookie

## 1.0.0.0 (UNRELEASED)

TL;DR:

* For server users, new features and configuration options have been
  added, but old configuration stays compatible. However some gophermap
  files may need adjusting, especially if they contain absolute paths
  not starting with a slash.
* For library users there are multiple braking changes to the core API
  that probably need adjusting in downstream usage as well as some
  changes to behavior.

### Server and Library Users

#### Gophermap parsing

There have been quite a few, partly breaking changes to gophermap parsing in
the library in an effort to fully support the format used in pygopherd and
bucktooth. Instances where spacecookie's parsing deviated from the established
format have been resolved and we now ship a test suite which checks compliance
against sample files from bucktooth and pygopherd.

We now support relative paths correctly: If a selector in a gophermap doesn't
start with `/` or `URL:` it is interpreted as a relative path and prefixed
with the directory the gophermap is located in. This should make writing
gophermaps much more convenient, as it isn't necessary to enter absolute
selectors anymore. However, absolute selectors not starting with `/`
are **broken** by this.

To facilitate these changes, the API of `Network.Gopher.Util.Gophermap`
changed in the following way:

* `GophermapEntry` changed to use `GophermapFilePath` instead of `FilePath`
  which may either be `GophermapAbsolute`, `GophermapRelative` or `GophermapUrl`.
  Additionally, `GophermapFilePath` is a wrapper around `RawFilePath`, contrary
  to the previous use of `FilePath`.
* `gophermapToDirectoryResponse` takes an additional parameter describing
  the directory the gophermap is located in to resolve relative to absolute
  selectors.

See also [#22](https://github.com/sternenseemann/spacecookie/issues/22) and
[#23](https://github.com/sternenseemann/spacecookie/pull/23).

Menu lines which only contain a file type and name are now required to be
terminated by a tab before the newline. This also reflects the behavior
of bucktooth and pygopherd (although the latter's documentation on this
is a bit misleading). Although this **breaks** entries like `0/file`,
info lines which start with a valid file type character like
`1. foo bar baz` no longer get mistaken for normal menu entries.
See [#34](https://github.com/sternenseemann/spacecookie/pull/34).

The remaining, less significant changes are:

* Fixed parsing of gophermap files whose last line isn't terminated
  by a newline.
* The `gophermaplineWithoutFileTypeChar` line type which mapped menu entries
  with incompatible file type characters to info lines has been removed. Such
  lines now result in a parse error. This is a **breaking change** if you
  relied on this behavior.
* `parseGophermap` now consumes the end of input.

#### Changes to Connection Handling

* We now wait up to 1 second for the client to close the connection on
  their side after sending all data. This fixes an issue specific to
  `curl` which would result in it failing with a recv error (exit code
  56) randomly.
  See also [#42](https://github.com/sternenseemann/spacecookie/issues/42)
  and [#44](https://github.com/sternenseemann/spacecookie/pull/44).
* Requests from clients are now checked more vigorously and limited
  in size and time to prevent denial of service attacks.
  * Requests may not exceed 1MB in size
  * The client is only given 10s to send its request
  * After the `\r\n` no additional data may be sent

### Server Users

#### Configuration

* Add new `listen` field to configuration allowing to specify the
  listening address and port. It expects an object with the fields
  `port` and `addr`. The top level `port` option has been *deprecated*
  as a result. It is now possible to bind to the link local address
  `::1` only without listening on public addresses.
  See [#13](https://github.com/sternenseemann/spacecookie/issues/13) and
  [#19](https://github.com/sternenseemann/spacecookie/pull/19).
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

Config parsing should be backwards compatible. Please open a bug report if
you experience any problems with that or any constellation of the new
settings.

#### Other changes

* A not allowed error is now generated if there are any dot directories or
  dot files along the path: `/foo/.dot/bar` would now generate an error
  instead of being processed like before.
* GHC RTS options are now enabled and the default option `-I10` is passed to
  spacecookie.
* Exit if dropping privileges fails instead of just logging an error like before.
  See [#45](https://github.com/sternenseemann/spacecookie/pull/45).
* Expand user documentation by adding three man pages
  ([rendered](https://sternenseemann.github.io/spacecookie/)) on the server daemon:
  * `spacecookie(1)`: daemon invocation and behavior
  * `spacecookie.json(5)`: daemon configuration
  * `spacecookie.gophermap(5)`: gophermap format documentation
* Fix the file not found error message erroneously stating that access of that
  file was not permitted.
* Clarify error message when an URL: selector is sent to spacecookie.
* Print version when `--version` is given
* Print simple usage instructions when `--help` is given or the command line
  can't be parsed.
* A warning is now logged when a gophermap file doesn't parse and the standard
  directory response is used as fallback.

### Library Users

### New Representation of Request and Response

The following changes are the most significant to the library as they
break virtually all downstream usage of spacecookie as a library.

The gopher request handler for the `runGopher`-variants now receives
a `GopherRequest` record representing the request instead of the
selector as a `String`. The upsides of this are as follows:

* Handlers now know the IPv6 address of the client in question
* Simple support for search transaction is introduced as the request
  sent by the client is split into selector and search string.
* Selectors are no longer required to be UTF-8 as `ByteString` is used.

If you want to reuse old handlers with minimal adjustments you can
use a snippet like the following. Note though that you might have
to make additional adjustments due to the changes to responses.

    wrapLegacyHandler :: (String -> GopherResponse)
                      -> (GopherRequest -> GopherResponse)
    wrapLegacyHandler f = f . uDecode . requestSelectorRaw

Corresponding to the switch to `ByteString` in `GopherRequest` the
whole API now uses `ByteString` to represent paths and selectors.
This prompts the following additional, breaking changes:

* `ErrorResponse` now uses a `ByteString` instead of a `String`.
* `GopherMenuItem`'s `Item` now uses a `ByteString` instead of a `FilePath`
  (you can use `encodeFilePath` from `filepath-bytestring` to fix downstream
  usage).
* `sanitizePath` and `sanitizeIfNotUrl` now operate on `RawFilePath`s
  (which is an alias for `ByteString`).
* As already mentioned, the gophermap API uses `RawFilePath`s instead
  of `FilePath`s as well.

See also [#38](https://github.com/sternenseemann/spacecookie/pull/38)
and [#26](https://github.com/sternenseemann/spacecookie/issues/26).

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
which may also be used by the user application (it is a simple `IO`
action). This additionally scales well: In the simplest case this could
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
was overly complicated and not as flexible as the new solution. Therefore
it was scrapped in favor of the new system.

#### Other Changes

* `cRunUserName` has been removed from `GopherConfig` since the functionality
  doesn't need special treatment as users can implement it easily via the
  ready action of `runGopherManual`. The formerly internal `dropPrivileges`
  function is now available via `Network.Gopher.Util` to be used for this
  purpose. See [#45](https://github.com/sternenseemann/spacecookie/pull/45).
  This is a **breaking change** and requires adjustment if you used the built
  in privilege deescalation capabilities.
* `santinizePath` and `santinizeIfNotUrl` have been corrected to `sanitizePath`
  and `sanitizeIfNotUrl` respectively. This is a **breaking change** to the
  interface of `Network.Gopher.Util`.

## 0.2.1.2 Bump fast-logger

2020-05-23

* Bump fast-logger dependency, fix build

## 0.2.1.1 Fixed Privilege Dropping

2019-12-10

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

2019-10-20

* Improved systemd support.
  * Support for the notify service type
  * Support for socket activation and socket (fd) storage
  * To make use of these new features you'll have to update your service files
* Added `defaultConfig` value to prevent future breakage in software using the
  library when the `GopherConfig` type is extended.
* Pretty print IPv6 addresses in logging

## 0.2.0.1 Hackage release

2019-05-23

Fixed a problem hindering the hackage release.

## 0.2.0.0 initial release

2019-05-23

* First version. Released on an unsuspecting world. Includes:
  * Library for writing any gopher server / application.
  * File system based gopher server with support for gopher maps.
  * Supports logging, privilege dropping, the gopher protocol and common extensions.
