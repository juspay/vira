# warp-tls-simple

Simple TLS certificate generation and configuration for Warp web server.

## Overview

`warp-tls-simple` provides automatic TLS certificate generation and management for [Warp](https://hackage.haskell.org/package/warp) web servers. It simplifies HTTPS setup for development and testing by automatically generating self-signed certificates with proper Subject Alternative Names (SAN) configuration.

## Features

- **Automatic Certificate Generation**: Creates self-signed TLS certificates if they don't exist
- **HTTP/2 Support**: Enables HTTP/2 over TLS for superior performance (especially for Server-Sent Events)
- **Development-Friendly**: Includes SAN entries for localhost, 127.0.0.1, and common local network ranges
- **OpenSSL Integration**: Uses system OpenSSL for certificate generation
- **Configuration Flexibility**: Supports both automatic and manual certificate management

## Why HTTPS for Development?

HTTP/2 provides significant performance benefits, especially for real-time features like Server-Sent Events (SSE). While HTTP/2 can technically work over HTTP, [in practice it requires HTTPS](https://http2-explained.haxx.se/en/part5#id-5.2.-http2-for-https) as most browsers only support HTTP/2 over TLS.

## Development Considerations

### Expected Browser Warnings

When using auto-generated self-signed certificates, you'll see browser warnings:

- **Chrome/Edge**: "Not secure" or `net::ERR_CERT_AUTHORITY_INVALID`
- **Firefox**: "Warning: Potential Security Risk Ahead"

This is normal and expected for self-signed certificates. The connection is still encrypted.

### Server-Side TLS Handshake Errors

You may see log entries like:

```
TLS handshake errors: HandshakeFailed (Error_Packet_unexpected "Alert13 [(AlertLevel_Fatal,CertificateUnknown)]")
```

These occur when clients (browsers, curl, etc.) reject the self-signed certificate. This is expected behavior and doesn't affect functionality.

### Solutions for Development

1. **Browser**: Click "Advanced" → "Proceed to localhost (unsafe)"
2. **curl**: Use `curl -k` to ignore certificate warnings
3. **Testing**: The auto-generated certificates work across your local network
4. **Production**: Replace with real certificates from a trusted CA (e.g., Let's Encrypt)

## Certificate Details

Auto-generated certificates include:

- **Subject**: `/CN=localhost`
- **Subject Alternative Names**:
  - `DNS:localhost`
  - `IP:127.0.0.1`
  - `IP:::1`
  - Common local network IP ranges for cross-device testing

## License

MIT
