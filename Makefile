all: image

static/elm.js: frontend/*.elm
	cd frontend ; elm-make Browse.elm --output elm.js
	mv frontend/elm.js static

static/browse.html: frontend/browse.html
	cp frontend/browse.html static

image: static/elm.js static/browse.html FORCE
	cd backend ; stack image container

stuff.tar.gz: FORCE
	tar -czf stuff.tar.gz scripts

FORCE:
