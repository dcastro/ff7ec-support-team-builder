################################################################################

.PHONY: build build-prod run test test-filter clean regen-weapons

build:
	npm run ts
	spago build

build-prod:
	find docs -maxdepth 1 -type f -delete
	rm -rf prod
	npm i
	mkdir -p prod
	cp dev/index.html prod/
	npm run ts
	spago bundle --outfile prod/index.js -p ff7ec-team-builder
	parcel build prod/index.html --dist-dir docs --public-url '.'

build-staging:
	rm -rf docs/staging
	rm -rf staging
	npm i
	mkdir -p staging
	cp dev/index.html staging/
	npm run ts
	spago bundle --outfile staging/index.js -p ff7ec-team-builder
	parcel build staging/index.html --dist-dir docs/staging --public-url '.'

run:
	make build
	npm run serve

# Runs benchmarks. The generated files can be uploaded to: https://harry.garrood.me/purescript-benchotron-svg-renderer/
bench:
	npm run ts
	spago run -m BenchMain -p ff7ec-team-builder-benchmarks

test:
	npm run ts
	spago test

# Usage:
#  * make test-filter FILTER="MyTest"
test-filter:
	npm run ts
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
