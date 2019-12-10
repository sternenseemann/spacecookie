# Revision history for spacecookie

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
