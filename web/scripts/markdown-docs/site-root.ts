import path from "path";
import { fileURLToPath } from "url";

const SCRIPT_DIR = path.dirname(fileURLToPath(import.meta.url));
export const WEB_PROJECT_ROOT_DIR = path.resolve(SCRIPT_DIR, "../..");
