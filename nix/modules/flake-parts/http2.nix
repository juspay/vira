{
  perSystem = { config, pkgs, ... }: {
    # Certificate generation for HTTP/2 TLS
    packages.generate-cert = pkgs.writeShellApplication {
      name = "generate-cert";
      runtimeInputs = [ pkgs.openssl ];
      text = ''
                set -euo pipefail
        
                CERT_DIR="''${1:-./tls}"
                mkdir -p "$CERT_DIR"
        
                # Generate private key
                if [ ! -f "$CERT_DIR/server.key" ]; then
                  echo "Generating private key..."
                  openssl genrsa -out "$CERT_DIR/server.key" 2048
                else
                  echo "Private key already exists."
                fi
        
                # Generate self-signed certificate directly (skip CSR)
                if [ ! -f "$CERT_DIR/server.crt" ]; then
                  echo "Generating self-signed certificate..."
                  openssl req -new -x509 -key "$CERT_DIR/server.key" -out "$CERT_DIR/server.crt" -days 365 \
                    -subj "/C=US/ST=CA/L=San Francisco/O=Vira Development/OU=IT Department/CN=localhost" \
                    -extensions v3_req -config <(cat <<EOF
        [req]
        distinguished_name = req_distinguished_name
        req_extensions = v3_req

        [req_distinguished_name]

        [v3_req]
        basicConstraints = CA:FALSE
        keyUsage = critical, digitalSignature, keyEncipherment
        extendedKeyUsage = critical, serverAuth
        subjectAltName = @alt_names

        [alt_names]
        DNS.1 = localhost
        DNS.2 = 127.0.0.1
        IP.1 = 127.0.0.1
        IP.2 = ::1
        EOF
        )
                else
                  echo "Certificate already exists."
                fi
        
                echo "TLS certificates generated in $CERT_DIR/"
                echo "  - Private key: $CERT_DIR/server.key"
                echo "  - Certificate: $CERT_DIR/server.crt"
      '';
    };

    # Helper script to setup certificates for development
    packages.setup-tls = pkgs.writeShellApplication {
      name = "setup-tls";
      runtimeInputs = [ config.packages.generate-cert ];
      text = ''
        echo "Setting up TLS certificates for Vira development..."
        generate-cert ./tls
        echo "TLS setup complete!"
        echo ""
        echo "Your server will now be available at:"
        echo "  https://localhost:5005"
        echo ""
        echo "Note: You'll need to accept the self-signed certificate in your browser."
      '';
    };
  };
}
