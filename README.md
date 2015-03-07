	 ____                                       _    _      
	/ ___| _ __   __ _  ___ ___  ___ ___   ___ | | _(_) ___ 
	\___ \| '_ \ / _` |/ __/ _ \/ __/ _ \ / _ \| |/ / |/ _ \
	 ___) | |_) | (_| | (_|  __/ (_| (_) | (_) |   <| |  __/
	|____/| .__/ \__,_|\___\___|\___\___/ \___/|_|\_\_|\___|
	      |_|    – haskell gopher server

## Gopher?

> The Gopher protocol /ˈɡoʊfər/ is a TCP/IP application layer protocol designed for distributing, searching, and retrieving documents over the Internet. The Gopher protocol was strongly oriented towards a menu-document design and presented an alternative to the World Wide Web in its early stages, but ultimately HTTP became the dominant protocol. The Gopher ecosystem is often regarded as the effective predecessor of the World Wide Web.

– [WP](https://en.wikipedia.org/wiki/Gopher_(protocol))

## Usage

	git clone https://github.com/lukasepple/spacecookie.git
	cd spacecookie
	./etc/build.sh # a convience wrapper around cabal commands
	cabal run spacecookie ROOTDIRECTORY
	# to install the binary system-wide in /usr/local/bin
	# use ./etc/install.sh
	# Please read it before using

spacecookie will now start serving `ROOTDIRECTORY`.

## To-Do

* ~~systemd unit file~~
* ~~Real systemwide installation~~
* ~~Threading~~
* ~~Byte count vs. Character count (utf-8)~~
* ~~Proper error handling whilst client handling~~ (Good enough for now)
* ~~Drop privileges~~
* Configuration (for replacing `serverName` and `serverPort` for example)
* Clean-up the code, refactor towards more pure code
