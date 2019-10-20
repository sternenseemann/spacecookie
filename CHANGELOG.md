# Revision history for spacecookie

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
