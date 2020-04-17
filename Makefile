ROOT_DIR:=$(shell dirname $(realpath $(firstword $(MAKEFILE_LIST))))
GEN:=$(ROOT_DIR)/gen
GEN_OPSGENIE_REST:=$(GEN)/opsgenie-rest
GEN_SWAGGER:=$(GEN)/swagger

test: patch-haskell
	cd $(GEN_OPSGENIE_REST) && stack test

patch-haskell: generate-haskell
	patch -p1 -i patches/0001-upgrade-to-lts-15.8.patch
	patch -p1 -i patches/0002-geniekey-scheme.patch

generate-haskell: patch-swagger
	rm -rf $(GEN_OPSGENIE_REST)
	mkdir -p $(GEN_OPSGENIE_REST)
	swagger-codegen generate -l haskell-http-client -i $(GEN_SWAGGER)/swagger.json -o $(GEN_OPSGENIE_REST)
	rm $(GEN_OPSGENIE_REST)/.gitignore
	rm $(GEN_OPSGENIE_REST)/.travis.yml

patch-swagger: generate-swagger
	patch -p1 -i patches/Add-global-consumes.patch

generate-swagger:
	rm -rf $(GEN_SWAGGER)
	mkdir -p $(GEN_SWAGGER)
	$(eval TMP := $(shell mktemp -d))
	cp -r --no-preserve=mode $(OPSGENIE_OAS)/* $(TMP)
	cd $(TMP) && node ./multi-file-swagger/index.js swagger.yaml alert > $(GEN_SWAGGER)/swagger.json
	rm -rf $(TMP)

node2nix:
	node2nix -i $(OPSGENIE_OAS)/multi-file-swagger/package.json -c nix/node.nix -o nix/node-packages.nix -e nix/node-env.nix

update:
	niv update
