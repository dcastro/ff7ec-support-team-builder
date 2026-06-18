install-deps:
    npm install -g esbuild spago@next parcel purescript purs-tidy

build:
    npm run ts
    spago build

build-prod:
    # The Google Sheets API key comes from `.env` (prod key); the private dev key lives only in the
    # gitignored `.env.development.local`, which `parcel build` (production mode) never loads.
    #
    # make sure the tests are passing
    just test
    # Clean the output directory
    find docs -maxdepth 1 -type f -delete
    rm -rf prod
    # Prepare
    npm i
    mkdir -p prod
    cp dev/index.html prod/
    # Build
    npm run ts
    spago bundle --outfile prod/index.js -p ff7ec-support-team-builder
    parcel build prod/index.html --dist-dir docs --public-url '.'

build-staging:
    # make sure the tests are passing
    just test
    rm -rf docs/staging
    rm -rf staging
    npm i
    mkdir -p staging
    cp dev/index.html staging/
    npm run ts
    spago bundle --outfile staging/index.js -p ff7ec-support-team-builder
    parcel build staging/index.html --dist-dir docs/staging --public-url '.'

run:
    just build
    npm run serve

# Runs benchmarks. The generated files can be uploaded to: https://harry.garrood.me/purescript-benchotron-svg-renderer/
bench:
    npm run ts
    spago run -m BenchMain -p ff7ec-support-team-builder-benchmarks

test:
    npm run ts
    spago test
    just check-effects

# Usage: just test-filter "MyTest"
test-filter filter:
    npm run ts
    spago test -- --example "{{ filter }}"
    just check-effects

format:
    purs-tidy format-in-place "src/**/*.purs" "test/**/*.purs"

# Clean all artifacts
clean:
    rm -rf output

full-clean:
    rm -rf output
    rm -rf node_modules
    rm -rf .spago
    rm -rf .psci_modules
    rm -rf .parcel-cache

repl:
    spago repl

# Check for weapon effects in weapons.json not yet handled by the parser
check-effects:
    spago run -m CheckEffects.Main -p ff7ec-support-team-builder > "resources/unsupported_effects.md"

# Regenerate the `weapons.json` file used in the tests
regen-weapons:
    # NOTE: you must set the `FF7EC_GOOGLE_SHEETS_KEY` environment variable with a Google API key first.
    curl \
    	"https://sheets.googleapis.com/v4/spreadsheets/1evoNzTA9veDRTvYJEMe-9F81QQ-CxUWN4mrd93kn2W4/values/Weapons!A%3AZZ?key=${FF7EC_GOOGLE_SHEETS_KEY}" \
    	--header 'Accept: application/json' \
    	--compressed > resources/weapons.json
    # update .snap files and unsupported_effects.md
    just test
