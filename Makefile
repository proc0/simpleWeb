.PHONY: clean deep-clean

clean:
	stack clean
	rm -rf out
	cd client && rm -rf dist

deep-clean: clean
	cd client && rm -rf node_modules

run: build
	cd out && ./server

build: out/ out/js/ out/js/bundle.js out/server out/server-configuration.cfg

out/:
	mkdir -p out

out/js/: out/
	mkdir -p out/js

out/js/bundle.js: client/package.json client/src
	cd client && npx webpack
	cp client/dist/bundle.js out/js/bundle.js

out/server: stack.yaml src/bin src/lib
	stack build
	cp $$(stack exec -- which server) out/server

out/server-configuration.cfg: server-configuration.cfg
	cp server-configuration.cfg out/
