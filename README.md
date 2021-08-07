# share-square

Webapp for balancing shared expenses written in Elm.

## Build

Install tools:

```shell
npm install
```

Compile the Elm sources into a single JavaScript file:

```shell
npm run build
```

The resulting file `app.js` is included (hard-coded) in `index.html`.

## Serve

Serve all files in the repository on port `8080`:

```shell
npx http-server
```

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

The Elm sources are formatted according to the whims of `elm-format`.
Run `npm run fmt` to reformat all Elm source files in the `src` folder:
