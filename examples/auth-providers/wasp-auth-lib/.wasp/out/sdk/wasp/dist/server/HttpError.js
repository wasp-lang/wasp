export class HttpError extends Error {
    statusCode;
    data;
    constructor(statusCode, message, data, options) {
        super(message, options);
        if (Error.captureStackTrace) {
            Error.captureStackTrace(this, HttpError);
        }
        this.name = this.constructor.name;
        if (!(Number.isInteger(statusCode) && statusCode >= 400 && statusCode < 600)) {
            throw new Error('statusCode has to be integer in range [400, 600).');
        }
        this.statusCode = statusCode;
        if (data) {
            this.data = data;
        }
    }
}
//# sourceMappingURL=HttpError.js.map