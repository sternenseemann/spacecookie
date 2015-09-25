	 ____                                       _    _      
	/ ___| _ __   __ _  ___ ___  ___ ___   ___ | | _(_) ___ 
	\___ \| '_ \ / _` |/ __/ _ \/ __/ _ \ / _ \| |/ / |/ _ \
	 ___) | |_) | (_| | (_|  __/ (_| (_) | (_) |   <| |  __/
	|____/| .__/ \__,_|\___\___|\___\___/ \___/|_|\_\_|\___|
	      |_|    – haskell gopher server

## Gopher?

> The Gopher protocol /ˈɡoʊfər/ is a TCP/IP application layer protocol designed for distributing, searching, and retrieving documents over the Internet. The Gopher protocol was strongly oriented towards a menu-document design and presented an alternative to the World Wide Web in its early stages, but ultimately HTTP became the dominant protocol. The Gopher ecosystem is often regarded as the effective predecessor of the World Wide Web.

– [WP](https://en.wikipedia.org/wiki/Gopher_(protocol))

## Installation and Usage

	git clone https://github.com/lukasepple/spacecookie.git
	cd spacecookie
	make
	sudo make install
	spacecookie <configfile> # /usr/local/etc/spacecookie.yaml

### Gophermaps
If you want to have something else than the automatically generated gopher menu you can use a [gophermap file](./docs/gophermap-pygopherd.txt) to describe the menu. Spacecookie will use it automatically if you save it as `.gophermap` in the corresponding directory. ([Example file](./docs/gophermap), [WP: Gophermap](https://en.wikipedia.org/wiki/Gophermap))

## To-Do

* ~~systemd unit file~~
* ~~Real systemwide installation~~
* ~~Threading~~
* ~~Byte count vs. Character count (utf-8)~~
* ~~Proper error handling whilst client handling~~ (Good enough for now)
* ~~Drop privileges~~
* ~~Clean-up the code, refactor towards more pure code~~
* ~~Add support for [gophermap files](https://en.wikipedia.org/wiki/Gophermap)~~
* chroot if possible
* test, test, test
* update systemd files and optimize system-wide experience
