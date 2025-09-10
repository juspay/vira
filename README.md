# vira

_/வீரா/_

No-frills CI for teams using Nix.

<img src="packages/vira/static/vira-logo.svg" style="height: 64px;" />

## Status

The project is currently in beta stage. Contact [@srid](https://github.com/srid) for details.

## Tech

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

## Roadmap

TBD

## Development

```sh
# Run the development server (HTTPS)
just run

# Or, if you need to start from empty database (useful if you have changed the acid-state types)
# NOTE: This won't delete TLS certs.
just resetdb run
```

### HTTPS and HTTP/2 Support

Vira uses HTTP/2 for superior SSE (log streaming) performance and automatically handles TLS certificate generation via the [`warp-tls-simple`](../warp-tls-simple/README.md) package.

**Key features:**

- **Automatic Certificate Generation**: Certificates are auto-generated in `./state/tls/` when you run Vira
- **Manual Certificate Support**: Use `--tls-cert` and `--tls-key` flags for custom certificates
- **Development URLs**: https://localhost:5005 (HTTPS) or http://localhost:5005 (with `--no-https`)

For detailed information about TLS configuration, certificate generation, troubleshooting browser warnings, and development considerations, see the [`warp-tls-simple` documentation](../warp-tls-simple/README.md).

## NixOS Module

Vira provides a NixOS module for easy deployment. See the [NixOS configuration example](nix/examples/nixos/flake.nix) for usage.

## Home Manager Module

Vira provides a Home Manager module for running Vira as a user service. Supports Linux (systemd user services) and macOS (launchd agents).

> **Warning**: The nix-darwin module is currently untested.

See the [Home Manager configuration example](nix/examples/home-manager/flake.nix) for usage.

## Beta Testing

```
nix --accept-flake-config run github:juspay/vira -- web --port 5005
```

You can now add your repositories in the web app.
