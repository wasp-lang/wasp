// Shorthand ambient wildcard: every wasp/* import typechecks as `any` unless
// a real declaration file resolves first (e.g. wasp/server/operations).
declare module "wasp/*";

// Side-effect CSS imports (`import "./X.css"`) carry no types.
declare module "*.css";
