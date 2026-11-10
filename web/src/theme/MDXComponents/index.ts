import InlineWaspCommand from "@site/src/components/InlineWaspCommand";
import PackageManagerCommand from "@site/src/components/PackageManagerCommand";
import WaspCommand from "@site/src/components/WaspCommand";
import MDXComponents from "@theme-original/MDXComponents";

// Components that every docs page can use without importing them.
export default {
  ...MDXComponents,
  InlineWaspCommand,
  PackageManagerCommand,
  WaspCommand,
};
