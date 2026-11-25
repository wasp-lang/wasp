import InlineWaspCommand from "@site/src/components/Commands/InlineWaspCommand";
import PackageManagerCommand from "@site/src/components/Commands/PackageManagerCommand";
import WaspCommand from "@site/src/components/Commands/WaspCommand";
import MDXComponents from "@theme-original/MDXComponents";

// Components that every docs page can use without importing them.
export default {
  ...MDXComponents,
  InlineWaspCommand,
  PackageManagerCommand,
  WaspCommand,
};
