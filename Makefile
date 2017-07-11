all: stuff.tar.gz

release/index.html:
	cd frontend ; elm-make foo.elm
	cp frontend/index.html release

stuff.tar.gz: release/index.html
	tar -czf stuff.tar.gz release

FORCE:
