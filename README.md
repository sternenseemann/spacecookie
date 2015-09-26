	 ____                                       _    _      
	/ ___| _ __   __ _  ___ ___  ___ ___   ___ | | _(_) ___ 
	\___ \| '_ \ / _` |/ __/ _ \/ __/ _ \ / _ \| |/ / |/ _ \
	 ___) | |_) | (_| | (_|  __/ (_| (_) | (_) |   <| |  __/
	|____/| .__/ \__,_|\___\___|\___\___/ \___/|_|\_\_|\___|
	      |_|    – haskell gopher server

## What is Gopher?

> The Gopher protocol /ˈɡoʊfər/ is a TCP/IP application layer protocol designed for distributing, searching, and retrieving documents over the Internet. The Gopher protocol was strongly oriented towards a menu-document design and presented an alternative to the World Wide Web in its early stages, but ultimately HTTP became the dominant protocol. The Gopher ecosystem is often regarded as the effective predecessor of the World Wide Web.

– [WP](https://en.wikipedia.org/wiki/Gopher_(protocol))

## What is Spacecookie?

## Spacecookie's features

## Installation

For starters, clone the git repository, build and install Spacecookie!

	git clone https://github.com/lukasepple/spacecookie.git
	cd spacecookie
	make
	sudo make install

## Configuration

In order to run your new gopher server, you got to configure it first. This is done using the file `/usr/local/etc/spacecookie.yaml` (but you can place an equivalent file anywhere in your file system). It looks like this:

	hostname: my.gopher.space
	user: gopher
	port: 70
	root: /srv/gopher

Let's have a quick look at the options:

option     | meaning                                                 
-----------|--------------------------------------------------------------------------------------------------------
`hostname` | The hostname your spacecookie will be reachable through.
`user`     | The user that just run spacecookie. It is used to drop root priveleges after binding the server socket.
`port`     | The port spacecookie should listen on. The well-known port for gopher is 70.
`root`     | The directory which the files to serve via gopher are located in.

## Running Spacecookie

After you've created your config file just start spacecookie like this:

	spacecookie /path/to/spacecookie.yaml

Of course it is more convenient to run it as a system wide demon. For that reason a systemd `spacecookie.service` is provided. You can use it like this:

	systemctl enable spacecookie.service
	systemctl start  spacecookie.service

Per default the service uses `/usr/local/etc/spacecookie.yaml` as configuration file.

## Adding Content

Having a gopher server is nice but not that nice without any nice content to serve. Luckily adding content is extremely easy as Spacecookie does nothing else than serving files beneath the `root`-directory (note that Spacecookie does _not_ serve files with names that start with a dot).

Directory listings are also generated automatically for you. If you want a fancy listning with future information and text you can use a [gophermap](https://en.wikipedia.org/wiki/Gophermap) to define a custom (static) listning for a directory. This is often used to create a nice "homepage", i. e. the root directory listning. Below is an example gophermap file.

	You can just start writing text that
	will be displayed by the gopher client
	without a link to a file. Empty lines are
	also possible.

	1Menu Entry for a directory full of funny stuff	/funny
	IFunny Image	/funy.jpg
	gcat gif	/cat.gif
	0about me	/about.txt
	1Floodgap's gopher server	/	gopher.floodgap.com	70

So what does that all mean? These are the rules for a gophermap file:

* comment lines (called info lines in Spacecookie's code) are just lines of text. They must not contain a tab! They will be displayed as lines of text by the gopher client.
* menu entries for files or directories start with a single char which specifies the file type, followed by the text for that file without a space or tab between them! Then the path is added after a tab.
* "Links" to other servers are like file/directory menu entries but the server's hostname and its port must be added (tab-separated).

The file type characters are defined in [RFC1435](https://tools.ietf.org/html/rfc1436#page-10). Detailed documentation on the gophermap format [can be found here](./docs/gophermap-pygopherd.txt) (taken from the pygopherd man page).

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
