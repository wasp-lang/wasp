import { heroui } from "@heroui/react";

// This export relies on a hidden type of a transitive dependency, so TypeScript
// cannot properly export the inferred type here. We don't use ever this export
// directly (we just pass it to Tailwind), so it's fine to cast it to unknown.
export default heroui() as unknown;
