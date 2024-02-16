export declare const hashPassword: (password: string) => Promise<string>;
export declare const verifyPassword: (hashedPassword: string, password: string) => Promise<void>;
