// @ts-check

import * as fs from "node:fs/promises";

/**
 * @param {string} filePath
 * @returns {Promise<boolean>}
 */
export async function doesPathExist(filePath) {
  return fs.access(filePath).then(
    () => true,
    () => false,
  );
}
