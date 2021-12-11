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

## Storage

The app currently offers the options of not persisting the state at all or persisting it in the Browser's local storage.
All the state is kept in a single field (`data`) as a single JSON document.
Due to reasonably tight (and seemingly non-uniform) space constraints,
an attempt has been made to make this document as small as possible.

The data is prefixed by the schema version (currently "1")
and separated from the data using a marker '|'.
This version is used to determine if the app will be able to decode the data
(and in the future determine which decoder to use).

Other from the data itself, a "revision" number is stored under another key `data@revision`.
This number is incremented on each write and is used to determine if changes have been made in another browser tab.
If so, the write is rejected.
Currently, this is only reported in the console log and the only way to recover
is to refresh the state by doing a full page reload.
