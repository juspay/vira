# HTTPS and HTTP/2 Support

Vira uses HTTP/2 for superior SSE (log streaming) performance and automatically handles TLS certificate generation via the [`warp-tls-simple`](https://github.com/juspay/vira/blob/main/packages/warp-tls-simple/README.md) package.

## Key features

- **Automatic Certificate Generation**: Certificates are auto-generated in `./state/tls/` when you run Vira
- **Manual Certificate Support**: Use `--tls-cert` and `--tls-key` flags for custom certificates
- **Development URLs**: https://localhost:5005 (HTTPS) or http://localhost:5005 (with `--no-https`)

For detailed information about TLS configuration, certificate generation, troubleshooting browser warnings, and development considerations, see the [`warp-tls-simple` documentation](https://github.com/juspay/vira/blob/main/packages/warp-tls-simple/README.md).
