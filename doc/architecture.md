# Architecture

## Tech stack

| Category      | Tools/Libraries                                                                                                                                                    |
| ------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| Frontend      | [HTMX](https://htmx.org/) (+ [hyperscript](https://hyperscript.org/) where relevant)                                                                               |
| Data Store    | [acid-state](https://github.com/acid-state/acid-state)                                                                                                             |
| Web Server    | [servant](https://www.servant.dev/) + [warp](https://hackage.haskell.org/package/warp) + [warp-tls](https://hackage.haskell.org/package/warp-tls) (HTTP/2 + HTTPS) |
| Effect System | [effectful](https://hackage.haskell.org/package/effectful)                                                                                                         |
| Logging       | [co-log](https://kowainik.github.io/projects/co-log)                                                                                                               |
| HTML DSL      | Lucid2                                                                                                                                                             |
| CSS           | TailwindCSS                                                                                                                                                        |
| Nix CI        | [Omnix](https://omnix.page/om/ci.html)                                                                                                                             |
