/** Identities are keyed by the normalized address: trimmed and lower-cased. */
export declare function normalizeEmail(email: string): string;
/** Identities are keyed by the normalized username: trimmed and lower-cased. */
export declare function normalizeUsername(username: string): string;
export declare const PASSWORD_FIELD = "password";
export declare function ensureValidEmail(args: object): void;
export declare function ensureValidUsername(args: object): void;
export declare function ensurePasswordIsPresent(args: object): void;
export declare function ensureValidPassword(args: object): void;
export declare function ensureTokenIsPresent(args: object): void;
export declare function throwValidationError(message: string): never;
