# vira

_/வீரா/_

No-frills CI for teams using Nix.

<img src="static/vira-logo.jpg" style="height: 64px;" />

## Status

The project is currently in beta stage. Contact [@srid](https://github.com/srid) for details.

## Tech

| Category      | Tools/Libraries                                             |
| ------------- | ----------------------------------------------------------- |
| Frontend      | [HTMX](https://htmx.org/) (+ [hyperscript](https://hyperscript.org/) where relevant)                                   |
| Data Store    | [acid-state](https://github.com/acid-state/acid-state) |
| Web Server    | [servant](https://www.servant.dev/) + [warp](https://hackage.haskell.org/package/warp) + [warp-tls](https://hackage.haskell.org/package/warp-tls) (HTTP/2 + HTTPS)                         |
| Effect System | [effectful](https://hackage.haskell.org/package/effectful)  |
| Logging       | [co-log](https://kowainik.github.io/projects/co-log)        |
| HTML DSL      | Lucid2                                                      |
| CSS           | TailwindCSS                                                 |
| Nix CI        | [Omnix](https://omnix.page/om/ci.html) |

## Roadmap

TBD

## Development

```sh
# Run the development server (HTTPS)
just run

# Or, if you need to start from empty database (useful if you have changed the acid-state types)
just resetdb run
```

### HTTPS and HTTP/2 Support

We use HTTP/2 for superior SSE (used in log streaming) performance, which [in practice requires HTTPS](https://http2-explained.haxx.se/en/part5#id-5.2.-http2-for-https). Vira automatically generates self-signed TLS certificates for HTTPS with HTTP/2 support:

1. **Automatic Certificate Generation**: 
   When you run `nix run github:juspay/vira`, certificates are automatically generated in `./state/tls/` if they don't exist.

2. **Manual Certificate Control** (optional):
   ```sh
   # Use your own certificates
   nix run github:juspay/vira -- --tls-cert /path/to/cert.crt --tls-key /path/to/private.key
   ```

3. **Development URLs**:
   - HTTPS: https://localhost:5005 (with auto-generated certificates)
   - HTTP: http://localhost:5005 (if you run via with `--no-https`)

The auto-generated certificates include Subject Alternative Names (SAN) for localhost, 127.0.0.1, and common local network IP ranges, making them suitable for local development and testing across your network.

#### Common TLS Development Issues

When using HTTPS with self-signed certificates, you may see:

1. **Browser Warnings**: "Not secure" or `net::ERR_CERT_AUTHORITY_INVALID` - this is normal for self-signed certificates
2. **Server Log Errors**: TLS handshake errors like `HandshakeFailed (Error_Packet_unexpected "Alert13 [(AlertLevel_Fatal,CertificateUnknown)]")` - these occur when clients reject the self-signed certificate

These are expected behaviors for development and don't affect functionality. The connection is still encrypted.

**Solutions:**
- **Accept in Browser**: Click "Advanced" → "Proceed to localhost (unsafe)"
- **Curl**: Use `curl -k` to ignore certificate warnings
- **Production**: Use real certificates from a trusted CA (e.g.: Let's Encrypt)

## NixOS Module

Vira provides a NixOS module for easy deployment. See the [NixOS configuration example](nix/examples/nixos/flake.nix) for usage.

## Beta Testing

```
nix run github:juspay/vira --accept-flake-config -- --port 5005
```

You can now add your repositories in the web app.