<!-- LLM PROMPT: This document contains all notes from an Emanote notebook.
Each note is separated by '===' delimiters and includes metadata headers.
- Source: The original file path in the notebook
- URL: The full URL where this note can be accessed
- Title: The note's title
- Wikilinks: All possible ways to reference this note using [[wikilink]] syntax

When referencing notes, you can use any of the wikilinks provided.
The base URL is: 
-->

<!-- Source: https.md -->
<!-- URL: /https -->
<!-- Title: HTTPS and HTTP/2 Support -->
<!-- Wikilinks: [[https]] -->

# HTTPS and HTTP/2 Support

Vira uses HTTP/2 for superior SSE (log streaming) performance and automatically handles TLS certificate generation via the [`warp-tls-simple`](https://github.com/juspay/vira/blob/main/packages/warp-tls-simple/README.md) package.

## Key features

- **Automatic Certificate Generation**: Certificates are auto-generated in `./state/tls/` when you run Vira
- **Manual Certificate Support**: Use `--tls-cert` and `--tls-key` flags for custom certificates
- **Development URLs**: https://localhost:5005 (HTTPS) or http://localhost:5005 (with `--no-https`)

For detailed information about TLS configuration, certificate generation, troubleshooting browser warnings, and development considerations, see the [`warp-tls-simple` documentation](https://github.com/juspay/vira/blob/main/packages/warp-tls-simple/README.md).


===

<!-- Source: index.md -->
<!-- URL: // -->
<!-- Title: Vira -->
<!-- Wikilinks: [[index]] -->

# Vira

No-frills CI for teams using Nix.

<img src="https://raw.githubusercontent.com/juspay/vira/refs/heads/main/packages/vira/static/vira-logo.svg" alt="Logo" style="height: 128px;" />

> [!warning]
> Vira is in active development. View the [GitHub repo](https://github.com/juspay/vira) for development progress.

## Getting Started

```sh
nix --accept-flake-config run github:juspay/vira -- web --port 5005
```

You can now add your repositories in the web app.

## Guide

For tech stack details, see [[tech]].

For HTTPS and HTTP/2 configuration details, see [[https]].

For NixOS and Home Manager module usage, see [[modules]].


===

<!-- Source: modules.md -->
<!-- URL: /modules -->
<!-- Title: Nix Modules -->
<!-- Wikilinks: [[modules]] -->

# Nix Modules

## NixOS Module

Vira provides a NixOS module for easy deployment. See the [NixOS configuration example](https://github.com/juspay/vira/blob/main/nix/examples/nixos/flake.nix) for usage.

## Home Manager Module

Vira provides a Home Manager module for running Vira as a user service. Supports Linux (systemd user services) and macOS (launchd agents).

> [!warning]
> The home-manager service may stop for any reason on macOS. You can bring it back by running:
>
> ```sh
> launchctl load ~/Library/LaunchAgents/org.vira.service.plist
> ```

See the [Home Manager configuration example](https://github.com/juspay/vira/blob/main/nix/examples/home-manager/flake.nix) for usage.


===

<!-- Source: tech.md -->
<!-- URL: /tech -->
<!-- Title: Tech Stack -->
<!-- Wikilinks: [[tech]] -->

# Tech Stack

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
