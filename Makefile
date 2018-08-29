all: image

static/elm.js: frontend/*.elm frontend/Client/Types.elm
	cd frontend ; elm-package install --yes
	cd frontend ; elm-make --yes Browse.elm --output elm.js
	mkdir -p static
	mv frontend/elm.js static

frontend/Client/Types.elm: backend/src/Types.hs backend/ElmGen.hs stack-build
	mkdir -p frontend/Client
	cd backend ; stack runghc ElmGen.hs

static/browse.html: frontend/browse.html
	mkdir -p static
	cp frontend/browse.html static

stack-build: FORCE
	cd backend ; stack build

image: static/elm.js static/browse.html stack-build
	cd backend ; stack image container

script-image: scripts/*
	cd scripts ; docker build -t kasti-scripts .

run_local: create_container
	docker restart kasti-container

create_container: image
	docker stop kasti-container
	docker rm kasti-container
	docker create --name kasti-container -p 3000:3000 kasti-backend:latest kasti-server /root/static/kasti-config.json

FORCE:
