import path from "path";
import { fileURLToPath } from "url";

const SCRIPT_DIR = path.dirname(fileURLToPath(import.meta.url));
export const SITE_ROOT_DIR = path.resolve(SCRIPT_DIR, "../..");
