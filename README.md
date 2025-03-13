# vira

_/வீரா/_

TBD

## Status

The project is currently in prototype phase. Contact [@srid](https://github.com/srid) for details.

## Tech

| Category      | Tools/Libraries                                             |
| ------------- | ----------------------------------------------------------- |
| Frontend      | [HTMX](https://htmx.org/) (+ [hyperscript](https://hyperscript.org/) where relevant)                                   |
| Data Store    | [acid-state](https://github.com/acid-state/acid-state) |
| Web Server    | [servant](https://www.servant.dev/)                         |
| Effect System | [effectful](https://hackage.haskell.org/package/effectful)  |
| Logging       | [co-log](https://kowainik.github.io/projects/co-log)        |
| HTML DSL      | Lucid2                                                      |
| CSS           | TailwindCSS                                                 |
| Nix CI        | [Omnix](https://omnix.page/om/ci.html) |

## Roadmap

TBD

## Development

```sh
just run

# Or, if you need to start from empty database (useful if you have changed the acid-state types)
just resetdb run
```

## Beta Testing

```
nix run github:juspay/vira -- --host <interface-ip> --port 5005
```

This uses samples repos, but you can pass your own in the command line.
