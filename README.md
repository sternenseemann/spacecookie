# spacecookie
spacecookie is a gopher server written in Haskell.

## Gopher?

> The Gopher protocol /ˈɡoʊfər/ is a TCP/IP application layer protocol designed for distributing, searching, and retrieving documents over the Internet. The Gopher protocol was strongly oriented towards a menu-document design and presented an alternative to the World Wide Web in its early stages, but ultimately HTTP became the dominant protocol. The Gopher ecosystem is often regarded as the effective predecessor of the World Wide Web.

– [WP](https://en.wikipedia.org/wiki/Gopher_(protocol))

## Usage

	git clone https://github.com/lukasepple/spacecookie.git
	cd spacecookie
	cabal run spacecookie ROOTDIRECTORY

spacecookie will now start serving `ROOTDIRECTORY`. Because spacecookie is still in a early development stage there are no fancy daemon features yet.

## To-Do

* Configuration (for replacing `serverName` and `serverPort` for example)
* systemd unit file
* Real systemwide installation
* Clean-up the code, refactor towards more pure code
* Proper error handling whilst client handling
* Threading
* Byte count vs. Character count (utf-8)
