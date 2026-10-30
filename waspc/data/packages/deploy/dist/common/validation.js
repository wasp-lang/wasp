import fs from "fs";
import path from "node:path";
export function assertDirPathIsAbsolute(dirPath, dirNameInError) {
    if (!path.isAbsolute(dirPath)) {
        throw new Error(`The ${dirNameInError} path must be absolute.`);
    }
}
export function assertDirExists(dirPath, dirNameInError) {
    const dirExists = fs.existsSync(dirPath);
    if (!dirExists) {
        throw new Error(`The ${dirNameInError} does not exist.`);
    }
}
//# sourceMappingURL=validation.js.map