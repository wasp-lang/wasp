// The npm scripts preload this file (`node -r`) after changing to the Wasp
// project dir. Requiring `dotenv` from here resolves it from the server
// package, no matter where npm installed it.
require("dotenv").config({ path: require("node:path").join(__dirname, ".env") });
