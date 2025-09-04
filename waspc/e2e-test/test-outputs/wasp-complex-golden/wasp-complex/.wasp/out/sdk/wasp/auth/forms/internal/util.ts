import { CSSProperties } from "react"

export const clsx = (...classes: (string | undefined)[]) => {
  return classes.filter(Boolean).join(" ");
};

export const tokenObjToCSSVars = (
  prefix: string,
  tokenObj: Record<string, string | number>,
): CSSProperties =>
  Object.fromEntries(
    Object.entries(tokenObj).map(([key, value]) => [
      `--${prefix}-${key}`,
      value,
    ]),
  );
