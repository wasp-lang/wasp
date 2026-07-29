// Declares the virtual user modules the SDK imports, so it can typecheck before
// the bundler resolves them into actual user files.
//
// The types are written as inline `import("...")` types on purpose. These are
// ambient module declarations, and those cannot reach another module through a
// relative import statement (TypeScript reports TS2439). `skipLibCheck` hides
// that error, so getting it wrong silently types every export below as `any`.
