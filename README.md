# share-square

Webapp for balancing shared expenses written in Elm.

## Build

Install tools:

```shell
npm install
```

Compile the Elm sources into a single JavaScript file:

```shell
npm run build -- --mode=development
```

If the `--mode` arg is omitted, it defaults to "production".

The resulting file `app.js` is included (hard-coded) in `index.html`.

## Serve

The following command builds the project and starts a hot-reloading server on port `8080`:

```shell
npm run serve -- --mode=development
```

The app will open in a browser automatically and the contents refresh whenever the source files change.

Minor caveat: Due to the way that they're initialized, tooltips will not behave properly
after a hot-reload (i.e. they will fall back to the way that the browser displays title text by default).

## Docker

Build image based on `nginx`:

```shell
docker build -t share-square:latest .
```

Serve on port `8080`:

```shell
docker run --rm -p 8080:80 share-square:latest
```

The npm script `npm run docker` runs both of the above commands.

## Development

### Formatting

The Elm sources are formatted according to the whims of `elm-format`.
Run `npm run fmt` to reformat all Elm source files in the `src` folder:
