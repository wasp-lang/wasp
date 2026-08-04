import fs from "node:fs";
import * as z from "zod";
import { CodexOutputSchema } from "./schema.mjs";

const outputPath = new URL("./output-schema.json", import.meta.url);
const outputSchema = z.toJSONSchema(CodexOutputSchema);

fs.writeFileSync(outputPath, `${JSON.stringify(outputSchema, null, 2)}\n`);
