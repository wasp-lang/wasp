import {
  JsonFileMailboxDirectory,
  type MailboxDirectory,
} from "./mailboxDirectory.js";
import {
  createMessage,
  summarizeMessage,
  type DevMailboxEmail,
  type DevMailboxMessage,
  type DevMailboxMessageSummary,
} from "./message.js";
import {
  matchesFilter,
  type DevMailboxMessageFilter,
} from "./messageFilter.js";
import {
  createMonotonicClock,
  generateMessageId,
  isMessageId,
} from "./messageId.js";

export type DevMailboxOptions = {
  directoryPath: string;
  /** `Infinity` keeps all messages. */
  maxMessages: number;
  now?: () => Date;
};

export type DevMailboxEvent =
  | { type: "added"; message: DevMailboxMessageSummary }
  | { type: "deleted"; id: string }
  | { type: "cleared" };

export type DevMailboxListener = (event: DevMailboxEvent) => void;

export type WaitForMessageOptions = {
  timeoutMs: number;
  signal?: AbortSignal;
};

const WATCH_DEBOUNCE_MS = 20;
const POLL_INTERVAL_MS = 500;

/**
 * Messages live in memory and in a directory. The directory survives restarts
 * and other processes write to it too (e.g. `wasp db seed`), so every read
 * syncs with it first.
 */
export interface DevMailbox {
  readonly directoryPath: string;
  add(email: DevMailboxEmail): DevMailboxMessage | undefined;
  list(
    filter?: DevMailboxMessageFilter,
    limit?: number,
  ): DevMailboxMessageSummary[];
  get(id: string): DevMailboxMessage | undefined;
  delete(id: string): boolean;
  clear(): void;
  waitFor(
    filter: DevMailboxMessageFilter,
    options: WaitForMessageOptions,
  ): Promise<DevMailboxMessage | undefined>;
  /**
   * Includes changes other processes make. Never calls the listener before
   * returning. Returns a function that unsubscribes.
   */
  subscribe(listener: DevMailboxListener): () => void;
}

export function createDevMailbox(options: DevMailboxOptions): DevMailbox {
  return new DirectorySyncedDevMailbox(options);
}

class DirectorySyncedDevMailbox implements DevMailbox {
  readonly directoryPath: string;
  readonly #maxMessages: number;
  readonly #directory: MailboxDirectory;
  readonly #getSentAt: () => Date;
  readonly #messages = new Map<string, DevMailboxMessage>();
  readonly #listeners = new Set<DevMailboxListener>();
  #stopWatching: (() => void) | undefined;
  #pendingSync: ReturnType<typeof setTimeout> | undefined;

  constructor(options: DevMailboxOptions) {
    ensureValidMaxMessages(options.maxMessages);
    this.directoryPath = options.directoryPath;
    this.#directory = new JsonFileMailboxDirectory(options.directoryPath);
    this.#maxMessages = options.maxMessages;
    this.#getSentAt = createMonotonicClock(options.now);
  }

  /** Returns `undefined` when `maxMessages` is `0`. */
  add(email: DevMailboxEmail): DevMailboxMessage | undefined {
    if (this.#maxMessages === 0) {
      return undefined;
    }
    const sentAt = this.#getSentAt();
    const message = createMessage(email, generateMessageId(sentAt), sentAt);
    this.#directory.writeMessage(message);
    this.#syncWithDirectory();
    return message;
  }

  /** Newest first. */
  list(
    filter: DevMailboxMessageFilter = {},
    limit: number = Infinity,
  ): DevMailboxMessageSummary[] {
    return this.#syncAndFindNewestFirst(filter)
      .slice(0, limit)
      .map(summarizeMessage);
  }

  get(id: string): DevMailboxMessage | undefined {
    this.#syncWithDirectory();
    return this.#messages.get(id);
  }

  /** Returns whether the message existed. */
  delete(id: string): boolean {
    if (!isMessageId(id) || this.get(id) === undefined) {
      return false;
    }
    this.#directory.deleteMessage(id);
    this.#syncWithDirectory();
    return true;
  }

  clear(): void {
    for (const id of this.#directory.listMessageIds()) {
      this.#directory.deleteMessage(id);
    }
    this.#messages.clear();
    this.#emit({ type: "cleared" });
  }

  /**
   * Resolves with the newest match, waiting for one if there is none yet.
   * Resolves with `undefined` on timeout or abort.
   */
  waitFor(
    filter: DevMailboxMessageFilter,
    options: WaitForMessageOptions,
  ): Promise<DevMailboxMessage | undefined> {
    return new Promise((resolve) => {
      let isSettled = false;
      let unsubscribe: (() => void) | undefined;
      const finish = (message: DevMailboxMessage | undefined) => {
        if (isSettled) {
          return;
        }
        isSettled = true;
        clearTimeout(timeout);
        options.signal?.removeEventListener("abort", onAbort);
        unsubscribe?.();
        resolve(message);
      };
      const onAbort = () => finish(undefined);
      const timeout = setTimeout(() => finish(undefined), options.timeoutMs);
      options.signal?.addEventListener("abort", onAbort);
      if (options.signal?.aborted) {
        finish(undefined);
        return;
      }

      // Subscribing first means a message arriving before the lookup isn't missed.
      unsubscribe = this.subscribe((event) => {
        if (event.type === "added") {
          finishWithNewestMatch(this.#findLoadedNewestFirst(filter));
        }
      });
      finishWithNewestMatch(this.#syncAndFindNewestFirst(filter));

      function finishWithNewestMatch(matches: DevMailboxMessage[]) {
        const newestMatch = matches[0];
        if (newestMatch !== undefined) {
          finish(newestMatch);
        }
      }
    });
  }

  subscribe(listener: DevMailboxListener): () => void {
    this.#listeners.add(listener);
    this.#startWatchingIfNeeded();
    return () => {
      this.#listeners.delete(listener);
      this.#stopWatchingIfUnused();
    };
  }

  #syncAndFindNewestFirst(
    filter: DevMailboxMessageFilter,
  ): DevMailboxMessage[] {
    this.#syncWithDirectory();
    return this.#findLoadedNewestFirst(filter);
  }

  #findLoadedNewestFirst(filter: DevMailboxMessageFilter): DevMailboxMessage[] {
    return [...this.#messages.values()]
      .filter((message) => matchesFilter(message, filter))
      .sort((a, b) => b.id.localeCompare(a.id));
  }

  /** Emits events only after the whole sync, so listeners see every new message. */
  #syncWithDirectory(): void {
    const idsOnDisk = new Set(
      this.#deleteOldestOverLimit(this.#directory.listMessageIds()),
    );

    const deletedIds = [...this.#messages.keys()].filter(
      (id) => !idsOnDisk.has(id),
    );
    for (const id of deletedIds) {
      this.#messages.delete(id);
    }

    const addedMessages = [...idsOnDisk]
      .filter((id) => !this.#messages.has(id))
      .sort()
      .map((id) => this.#directory.readMessage(id))
      .filter((message) => message !== undefined);
    for (const message of addedMessages) {
      this.#messages.set(message.id, message);
    }

    for (const id of deletedIds) {
      this.#emit({ type: "deleted", id });
    }
    for (const message of addedMessages) {
      this.#emit({ type: "added", message: summarizeMessage(message) });
    }
  }

  #deleteOldestOverLimit(idsOldestFirst: string[]): string[] {
    const excessCount = Math.max(idsOldestFirst.length - this.#maxMessages, 0);
    for (const id of idsOldestFirst.slice(0, excessCount)) {
      this.#directory.deleteMessage(id);
    }
    return idsOldestFirst.slice(excessCount);
  }

  #emit(event: DevMailboxEvent): void {
    for (const listener of this.#listeners) {
      listener(event);
    }
  }

  #startWatchingIfNeeded(): void {
    if (this.#stopWatching !== undefined) {
      return;
    }
    const stopWatchingDirectory = this.#directory.watch(() =>
      this.#scheduleSync(),
    );
    // File watchers can miss events, e.g. right after they start on macOS.
    const poll = setInterval(() => this.#syncWithDirectory(), POLL_INTERVAL_MS);
    poll.unref();
    this.#stopWatching = () => {
      stopWatchingDirectory();
      clearInterval(poll);
    };
  }

  #stopWatchingIfUnused(): void {
    if (this.#listeners.size > 0 || this.#stopWatching === undefined) {
      return;
    }
    this.#stopWatching();
    this.#stopWatching = undefined;
    clearTimeout(this.#pendingSync);
    this.#pendingSync = undefined;
  }

  #scheduleSync(): void {
    clearTimeout(this.#pendingSync);
    this.#pendingSync = setTimeout(() => {
      this.#pendingSync = undefined;
      this.#syncWithDirectory();
    }, WATCH_DEBOUNCE_MS);
  }
}

function ensureValidMaxMessages(maxMessages: number): void {
  const isValid =
    maxMessages === Infinity ||
    (Number.isInteger(maxMessages) && maxMessages >= 0);
  if (!isValid) {
    throw new Error(
      `Dev mailbox maxMessages must be a non-negative integer or Infinity, got ${maxMessages}.`,
    );
  }
}
