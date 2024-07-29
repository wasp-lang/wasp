export declare function hashPassword(password: string): Promise<string>;
export declare function verifyPassword(hashedPassword: string, password: string): Promise<void>;
