{{={= =}=}}
/**
 * Declares the virtual user modules the SDK imports.
 * 
 * The types are written as inline `import("...")` types on purpose.
 * Ambient module declarations can't reach another module through a
 * relative import statement (TS2439).
 */
{=# virtualUserModules =}

declare module "{=& virtualModuleId =}" {
  {=# isDefaultExport =}
  const _default: {=& declaredType =};
  export default _default;
  {=/ isDefaultExport =}
  {=^ isDefaultExport =}
  export const {=& exportName =}: {=& declaredType =};
  {=/ isDefaultExport =}
}
{=/ virtualUserModules =}
