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

Minor caveat: Tabbed content will revert to their initial state on reload.

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

### Bootstrap integration

The layout of the app is set up as a plain
[Bootstrap](https://getbootstrap.com/docs/5.1/getting-started/introduction/)
application. It integrates the modal, tabs, and tooltips components
which are managed by the Javascript libraries that ship as part of Bootstrap.
The Elm code only sets up the initial HTML for these components
and then leaves it to these libraries to modify the nodes (e.g. set classes) appropriately.
This is safe as long as the virtual DOM doesn't change for any nodes that Bootstrap is managing
as that would cause Elm to replace them and Bootstrap to lose track.

This is admittedly a bit fragile, especially during development where hot-reloads do refresh the DOM
(at least the tabs are affected by this).
But porting a large chunk of Bootstrap's Javascript to Elm or littering the code with ports
is not an option for this project.

These are the reasons for the caveat mentioned [above](#serve).
