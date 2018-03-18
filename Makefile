all: image

static/elm.js: frontend/*.elm
	cd frontend ; elm-make --yes Browse.elm --output elm.js
	mkdir -p static
	mv frontend/elm.js static

static/browse.html: frontend/browse.html
	mkdir -p static
	cp frontend/browse.html static

image: static/elm.js static/browse.html FORCE
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
