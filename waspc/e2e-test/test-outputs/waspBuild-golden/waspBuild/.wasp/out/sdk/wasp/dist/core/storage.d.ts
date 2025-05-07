export type DataStore = {
    getPrefixedKey(key: string): string;
    set(key: string, value: unknown): void;
    get(key: string): unknown;
    remove(key: string): void;
    clear(): void;
};
export declare const storage: DataStore;
//# sourceMappingURL=storage.d.ts.map