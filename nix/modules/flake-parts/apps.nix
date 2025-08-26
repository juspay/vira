{ inputs, ... }:
{
  perSystem = { pkgs, ... }: {
    apps = {
      # Tabler Icons code generation app
      tabler-codegen.program = pkgs.writeShellApplication {
        name = "tabler-codegen";
        runtimeInputs = [ pkgs.haskellPackages.ghc ];
        text = ''
          set -euo pipefail
        
          TABLER_PATH="${inputs.tabler-icons}"
          OUTPUT_DIR="packages/tabler-icons/src/Web/TablerIcons"
        
          echo "Generating Tabler Icons bindings..."
          echo "Tabler path: $TABLER_PATH"
          echo "Output directory: $OUTPUT_DIR"
        
          # Create output directory
          mkdir -p "$OUTPUT_DIR"
        
          # Run the Haskell code generator
          runghc packages/tabler-icons/codegen.hs "$TABLER_PATH" "$OUTPUT_DIR"
        
          echo "Generated Tabler Icons bindings successfully!"
        '';
      };
    };
  };
}
