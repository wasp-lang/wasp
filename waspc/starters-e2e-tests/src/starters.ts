export const STARTER_NAMES = ["minimal", "basic"] as const;
export type StarterName = (typeof STARTER_NAMES)[number];
