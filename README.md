# vira

_/வீரா/_

No-frills CI for teams using Nix.

<img src="https://raw.githubusercontent.com/juspay/vira/refs/heads/main/packages/vira/static/vira-logo.svg" style="height: 64px;" />

## TASKS

- [ ] `vira ci` generates `./result` for _final_ build flake.
- ...

## Usage

> [!WARNING]
> The project is currently in beta stage. Contact [@srid](https://github.com/srid) for details. See <https://vira.nixos.asia/> for full documentation.

```sh
nix --accept-flake-config run github:juspay/vira -- web --port 5005
```

You can now add your repositories in the web app.

## Development

```sh
# Run the development server (HTTPS)
just run

# Or, if you need to start from empty database (useful if you have changed the acid-state types)
# NOTE: This won't delete TLS certs.
just resetdb run
```

## Contributing

See <https://vira.nixos.asia/contrib>
