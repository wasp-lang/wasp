// The project imports the Wasp Spec as `wasp/spec`, an alias that
// `tsconfig.paths.json` of this package maps to this file.
//
// The Wasp Spec lives in `@wasp/spec`, which the project doesn't list. Only
// some package managers let a project import a package it doesn't list, but
// all of them let `@wasp/wasp` import its own dependency.
export * from "@wasp/spec";
