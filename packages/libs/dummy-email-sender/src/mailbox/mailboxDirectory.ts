import * as fs from "node:fs";
import * as path from "node:path";

import type { DevMailboxMessage } from "./message.js";
import { isMessageId } from "./messageId.js";

const MESSAGE_FILE_EXTENSION = ".json";

/**
 * One `<id>.json` file per message. Writes go to a temporary file that is
 * then renamed, so readers never see half a message.
 */
export class MailboxDirectory {
  readonly path: string;

  constructor(directoryPath: string) {
    this.path = directoryPath;
  }

  listMessageIds(): string[] {
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
      .filter((fileName) => fileName.endsWith(MESSAGE_FILE_EXTENSION))
      .map((fileName) => fileName.slice(0, -MESSAGE_FILE_EXTENSION.length))
      .filter(isMessageId)
      .sort();
  }

  /** Returns `undefined` for missing or unreadable files. */
  readMessage(id: string): DevMailboxMessage | undefined {
    try {
      const content = fs.readFileSync(this.getMessageFilePath(id), "utf8");
      return JSON.parse(content) as DevMailboxMessage;
    } catch {
      return undefined;
    }
  }

  writeMessage(message: DevMailboxMessage): void {
    fs.mkdirSync(this.path, { recursive: true });
    const filePath = this.getMessageFilePath(message.id);
    const temporaryFilePath = path.join(
      this.path,
      `.${message.id}${MESSAGE_FILE_EXTENSION}.tmp`,
    );
    fs.writeFileSync(temporaryFilePath, JSON.stringify(message, null, 2));
    fs.renameSync(temporaryFilePath, filePath);
  }

  deleteMessage(id: string): void {
    fs.rmSync(this.getMessageFilePath(id), { force: true });
  }

  /** The watcher doesn't keep the process alive. */
  watch(onChange: () => void): () => void {
    fs.mkdirSync(this.path, { recursive: true });
    const watcher = fs.watch(this.path, { persistent: false }, onChange);
    watcher.on("error", () => watcher.close());
    return () => watcher.close();
  }

  private getMessageFilePath(id: string): string {
    if (!isMessageId(id)) {
      throw new Error(`Invalid dev mailbox message id: ${id}`);
    }
    return path.join(this.path, `${id}${MESSAGE_FILE_EXTENSION}`);
  }
}

function isFileNotFoundError(error: unknown): boolean {
  return (
    error instanceof Error && (error as NodeJS.ErrnoException).code === "ENOENT"
  );
}
