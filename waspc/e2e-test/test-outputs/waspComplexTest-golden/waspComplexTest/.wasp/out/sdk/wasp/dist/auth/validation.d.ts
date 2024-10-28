export declare const PASSWORD_FIELD = "password";
export declare function ensureValidEmail<Args extends object>(args: Args): void;
export declare function ensureValidUsername<Args extends object>(args: Args): void;
export declare function ensurePasswordIsPresent<Args extends object>(args: Args): void;
export declare function ensureValidPassword<Args extends object>(args: Args): void;
export declare function ensureTokenIsPresent<Args extends object>(args: Args): void;
export declare function throwValidationError(message: string): void;
