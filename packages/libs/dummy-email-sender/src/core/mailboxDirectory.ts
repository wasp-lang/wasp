import * as fs from "node:fs";
import * as path from "node:path";

import type { DevMailboxEmail } from "./email.js";
import { isEmailId } from "./emailId.js";

const EMAIL_FILE_EXTENSION = ".json";

export interface MailboxDirectory {
  readonly path: string;
  /** Oldest first. */
  listEmailIds(): string[];
  /** Returns `undefined` for missing or unreadable files. */
  readEmail(id: string): DevMailboxEmail | undefined;
  writeEmail(email: DevMailboxEmail): void;
  deleteEmail(id: string): void;
  /**
   * Calls `onChange` when files change. Returns a function that stops
   * watching. The watcher doesn't keep the process alive.
   */
  watch(onChange: () => void): () => void;
}

/**
 * One `<id>.json` file per email. Writes go to a temporary file that is
 * then renamed, so readers never see half a email.
 */
export class JsonFileMailboxDirectory implements MailboxDirectory {
  readonly path: string;

  constructor(directoryPath: string) {
    this.path = directoryPath;
  }

  listEmailIds(): string[] {
    let fileNames: string[];
    try {
      fileNames = fs.readdirSync(this.path);
    } catch (error) {
      if (isFileNotFoundError(error)) {
        return [];
      }
      throw error;
    }
    return fileNames
      .filter((fileName) => fileName.endsWith(EMAIL_FILE_EXTENSION))
      .map((fileName) => fileName.slice(0, -EMAIL_FILE_EXTENSION.length))
      .filter(isEmailId)
      .sort();
  }

  readEmail(id: string): DevMailboxEmail | undefined {
    try {
      const content = fs.readFileSync(this.getEmailFilePath(id), "utf8");
      return JSON.parse(content) as DevMailboxEmail;
    } catch {
      return undefined;
    }
  }

  writeEmail(email: DevMailboxEmail): void {
    fs.mkdirSync(this.path, { recursive: true });
    const filePath = this.getEmailFilePath(email.id);
    const temporaryFilePath = path.join(
      this.path,
      `.${email.id}${EMAIL_FILE_EXTENSION}.tmp`,
    );
    fs.writeFileSync(temporaryFilePath, JSON.stringify(email, null, 2));
    fs.renameSync(temporaryFilePath, filePath);
  }

  deleteEmail(id: string): void {
    fs.rmSync(this.getEmailFilePath(id), { force: true });
  }

  watch(onChange: () => void): () => void {
    fs.mkdirSync(this.path, { recursive: true });
    const watcher = fs.watch(this.path, { persistent: false }, onChange);
    watcher.on("error", () => watcher.close());
    return () => watcher.close();
  }

  private getEmailFilePath(id: string): string {
    if (!isEmailId(id)) {
      throw new Error(`Invalid dev mailbox email id: ${id}`);
    }
    return path.join(this.path, `${id}${EMAIL_FILE_EXTENSION}`);
  }
}

function isFileNotFoundError(error: unknown): boolean {
  return (
    error instanceof Error && (error as NodeJS.ErrnoException).code === "ENOENT"
  );
}
