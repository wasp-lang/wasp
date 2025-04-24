export declare const tokenStore: {
    createToken: (userId: string) => Promise<string>;
    verifyToken: (token: string) => Promise<{
        id: string;
    }>;
    isUsed: (token: string) => boolean;
    markUsed: (token: string) => void;
};
//# sourceMappingURL=oneTimeCode.d.ts.map