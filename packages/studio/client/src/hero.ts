import { heroui } from "@heroui/react";

// This export relies on a hidden type of a transitive dependency, so TypeScript
// cannot properly export the inferred type here. It complains in `composite:
// true` mode.
// https://github.com/heroui-inc/heroui/issues/6083
// As a workaround, we cast to `unknown`.
export default heroui() as unknown;
