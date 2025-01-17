################################################################################

.PHONY: build build-prod run test test-filter clean regen-weapons

build:
	spago build

build-prod:
	rm -rf docs
	rm -rf prod
	npm i
	mkdir -p prod
	cp dev/index.html prod/
	spago bundle --outfile prod/index.js
	parcel build prod/index.html --dist-dir docs --public-url '.'

build-staging:
	rm -rf docs/staging
	rm -rf prod
	npm i
	mkdir -p prod
	cp dev/index.html prod/
	spago bundle --outfile prod/index.js
	parcel build prod/index.html --dist-dir docs/staging --public-url '.'

run:
	npm run serve

test:
	spago test

# Usage:
#  * make test-filter FILTER="MyTest"
test-filter:
	spago test -- --example "$(FILTER)"

format:
	purs-tidy format-in-place "src/**/*.purs" "test/**/*.purs"

clean: ## Clean all artifacts
	rm -rf output

full-clean:
	rm -rf output
	rm -rf node_modules
	rm -rf .spago
	rm -rf .psci_modules
	rm -rf .parcel-cache


regen-weapons: ## Regenerate the `weapons.json` file used in the tests
	curl \
		'https://sheets.googleapis.com/v4/spreadsheets/1evoNzTA9veDRTvYJEMe-9F81QQ-CxUWN4mrd93kn2W4/values/Weapons!A%3AZ?key=AIzaSyARUnvuw1DvqJRISnPyOLEkqPvra4MF6fQ' \
		--header 'Accept: application/json' \
		--compressed > resources/weapons.json

################################################################################

help:	## Display this message
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'
.DEFAULT_GOAL := help
