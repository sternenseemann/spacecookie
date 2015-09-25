CORES=2
PREFIX=/usr/local

build: deps
	cabal build


rebuild: clean build

clean:
	cabal sandbox delete
	rm -rf dist

sandbox:
	cabal sandbox init

deps: sandbox
	cabal install --only-dependencies -j$(CORES)

install: build
	install -D -m755 dist/build/spacecookie/spacecookie $(PREFIX)/bin/spacecookie
	install -D -m644 etc/config.yaml $(PREFIX)/etc/spacecookie.yaml
	install -D -m644 LICENSE $(PREFIX)/share/licenses/spacecookie/LICENSE

uninstall:
	rm -rf $(PREFIX)/share/licenses/spacecookie
	rm $(PREFIX)/bin/spacecookie
	rm $(PREFIX)/etc/spacecookie.yaml
