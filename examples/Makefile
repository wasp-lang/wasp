
.PHONY: buildTodoApp
buildTodoApp: 
	cd todoApp/out && npm install && npm run-script build

.PHONY: deploy
deploy: buildTodoApp
# NOTE: Right now we copy todoApp directly to dist, but in the future we will probably
#   want to copy it to dist/todoApp, so we can have other examples next to it, all hosted together.
	rm -r dist
	cp -R todoApp/out/build dist
	firebase deploy
