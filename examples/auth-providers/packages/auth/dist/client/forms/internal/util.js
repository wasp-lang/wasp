export const clsx = (...classes) => classes.filter(Boolean).join(" ");
export const tokenObjToCSSVars = (prefix, tokenObj) => Object.fromEntries(Object.entries(tokenObj).map(([key, value]) => [
    `--${prefix}-${key}`,
    value,
]));
