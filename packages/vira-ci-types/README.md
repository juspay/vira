# vira-ci-types

This package contains CI-specific types and data structures for vira. It is separated into its own package to allow importing in hint configuration files without bringing in the entire vira dependency tree.

The package includes:

- `Vira.CI.Context` - CI context types
- `Vira.CI.Pipeline.Type` - Pipeline type definitions

This lightweight package enables hint-based configuration while maintaining clean dependency boundaries.
