# FF7 Ever Crisis - Team Builder

## Development

Requires Node.js and `npm`.

```sh
npm install -g purescript spago parcel esbuild
```

To launch the app in the browser:

```sh
make run
```

## Production

Run `make build-prod` to build the minified production bundle, you can find it in the `/docs` folder.

To test the production output locally, run:

```sh
npm install -g http-server
http-server docs -o
```
