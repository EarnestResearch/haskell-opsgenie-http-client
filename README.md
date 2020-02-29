# Haskell Opsgenie REST Client

Provides a Haskell client for the [Opsgenie REST API] based on a [http-client] backend.
To keep the size reasonable, a partial swagger definition is generated from the [Opsgenie Open API Specification]).
Currently, only the [Alert API] is generated.

## Development

### Dependencies

The nix-shell provides all necessary Node, Java, and Haskell dependencies to regenerate the project.
To start developing, [install Nix] and run `nix-shell`.
For convenience, there is a direnv wrapper around the nix-shell.

### Regnerate the client

To regenerate the client, run `make test`.  This will:

1. Generate a swagger.json into `gen` from the [Opsgenie OpenAPI Specification].
1. Generate the haskell client into `gen/opsgenie-rest`.
1. Apply patches to upgrade the LTS in the generated build
1. Run `stack test` to compile and test the code. At present, the generated code has no tests.

If the test passes, you can commit all the changes in the `gen` directory.

### Update Node dependencies

Generation of the swagger.json requires several Node dependencies.
To avoid npm, we use node2nix.
Run `make node2nix` to refresh the Node environment.
The generated files go into `nix/`.
Please be sure to run `make test` and commit the contents of `gen/` along with `nix/`.

### Update other dependencies

Run `niv update` to get the latest version of the Opsgenie OpenAPI Specification or [er-nix].
Please be sure to run `make test` and commit the contents of `gen/` along with `nix/sources.json`.

[Opsgenie REST API]: https://docs.opsgenie.com/docs/api-overview
[http-client]: https://hackage.haskell.org/package/http-client
[Opsgenie OpenAPI Specification]: https://github.com/opsgenie/opsgenie-oas/
[Alert API]: https://docs.opsgenie.com/docs/alert-api
[install Nix]: https://nixos.org/nix/download.html
[er-nix]: https://github.com/earnestresearch/er-nix
