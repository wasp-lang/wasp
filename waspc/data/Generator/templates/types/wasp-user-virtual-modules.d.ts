
// Makes the user project includes the Wasp virtual user modules declaration file.
// This is necessary to properly resolve the conditional types (e.g. `UserClientEnvSchema`) in the user project.
// Otherwise they will default to the `any` type.

/// <reference types="wasp/user-virtual-modules" />
