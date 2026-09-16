export declare class HttpError extends Error {
    statusCode: number;
    data: unknown;
    constructor(statusCode: number, message?: string, data?: Record<string, unknown>, options?: ErrorOptions);
}
//# sourceMappingURL=HttpError.d.ts.map