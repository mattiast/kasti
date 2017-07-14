all: stuff.tar.gz

release/index.html: frontend/foo.elm
	cd frontend ; elm-make foo.elm --output index.html
	cp frontend/index.html release

release/browse.html: frontend/Browse.elm
	cd frontend ; elm-make Browse.elm --output browse.html
	cp frontend/browse.html release

release/kasti-server: FORCE
	cd backend ; stack build
	cp backend/.stack-work/dist/x86_64-linux/Cabal-1.24.2.0/build/kasti-server/kasti-server release

stuff.tar.gz: release/index.html release/browse.html release/kasti-server
	tar -czf stuff.tar.gz release

FORCE:
