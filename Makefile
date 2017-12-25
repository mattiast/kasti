all: stuff.tar.gz

release/elm.js: frontend/*.elm
	cd frontend ; elm-make Browse.elm --output elm.js
	mkdir -p release
	mv frontend/elm.js release

release/browse.html: frontend/browse.html
	mkdir -p release
	cp frontend/browse.html release

release/kasti-server: FORCE
	cd backend ; stack build
	mkdir -p release
	cp backend/.stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/kasti-server/kasti-server release

release/scripts: FORCE
	mkdir -p release
	cp -R scripts release

stuff.tar.gz: release/elm.js release/browse.html release/kasti-server release/scripts
	tar -czf stuff.tar.gz release

FORCE:
