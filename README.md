# Savanna Model website builder

Hakyll project for building the Savanna Model static site

## Build

### Semantic UI and Gulp dependencies

```
$ npm install
$ cd semantic
$ gulp build
```

Use `gulp watch` for frequent changes to Semantic UI source files.

### Hakyll site exectuable

```
$ stack build
```

Use `stack build --fast --haddock-deps --file-watch` for active development on `site.hs` file.

### Generate Savanna Model site

```
$ stack exec -- site build
```

Use `stack exec -- site watch` for active development on template files.

## Preview

```
$ stack exec -- site watch
```

Then visit http://127.0.0.1:8000 to view the generated site.
