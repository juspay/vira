# vira

_/வீரா/_

TBD

<img src="static/vira-logo.jpg" style="height: 64px;" />

## Status

The project is currently in prototype phase. Contact [@srid](https://github.com/srid) for details.

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
# Run the development server (HTTP only by default)
just run

# Or, if you need to start from empty database (useful if you have changed the acid-state types)
just resetdb run
```

### HTTPS and HTTP/2 Support

Vira automatically generates self-signed TLS certificates for HTTPS with HTTP/2 support:

1. **Automatic Certificate Generation**: 
   When you run `nix run github:juspay/vira`, certificates are automatically generated in `./state/tls/` if they don't exist.

2. **Manual Certificate Control** (optional):
   ```sh
   # Use your own certificates
   cabal run vira -- --tls-cert /path/to/cert.crt --tls-key /path/to/private.key
   ```

3. **Development URLs**:
   - HTTP: http://localhost:5005
   - HTTPS: https://localhost:5005 (with auto-generated certificates)

The auto-generated certificates include Subject Alternative Names (SAN) for localhost, 127.0.0.1, and common local network IP ranges, making them suitable for local development and testing across your network.

### Common TLS Development Issues

When using HTTPS with self-signed certificates, you may see:

1. **Browser Warnings**: "Not secure" or `net::ERR_CERT_AUTHORITY_INVALID` - this is normal for self-signed certificates
2. **Server Log Errors**: TLS handshake errors like `HandshakeFailed (Error_Packet_unexpected "Alert13 [(AlertLevel_Fatal,CertificateUnknown)]")` - these occur when clients reject the self-signed certificate

These are expected behaviors for development and don't affect functionality. The connection is still encrypted.

**Solutions:**
- **Accept in Browser**: Click "Advanced" → "Proceed to localhost (unsafe)"
- **Curl**: Use `curl -k` to ignore certificate warnings
- **Production**: Use real certificates from a trusted CA

3. **Run with HTTP only** (default):
   ```sh
   # No TLS arguments - runs HTTP only
   cabal run vira
   ```

4. **CLI Arguments**:
   - `--tls-cert PATH`: Path to TLS certificate file
   - `--tls-key PATH`: Path to TLS private key file
   - Both arguments must be provided together to enable HTTPS
   - When both are provided, the server runs with HTTPS and HTTP/2

5. **Production certificates**:
   - Replace the self-signed certificates with proper certificates from a CA
   - Or use Let's Encrypt for automatic certificate management

6. **VSCode Remote Development**:
   - When using VSCode remote (e.g., from macOS to Linux), you may need to access the server via the host machine's IP
   - The generated certificate includes `localhost`, `127.0.0.1`, and `::1`
   - If accessing via a different IP, you'll get a certificate name mismatch warning (safe to ignore for development)

The server behavior is now explicitly controlled via CLI arguments rather than auto-detection.

## Beta Testing

```
nix run github:juspay/vira --accept-flake-config -- --host <interface-ip> --port 5005
```

This uses samples repos, but you can pass your own in the command line.
