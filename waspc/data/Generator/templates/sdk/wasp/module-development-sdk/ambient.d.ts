// Host-specific SDK exports are generated only after a module is installed in
// an app. Treat them as `any` while developing the module.
declare module "wasp/*";

// Side-effect CSS imports (`import "./X.css"`) carry no types.
declare module "*.css";
