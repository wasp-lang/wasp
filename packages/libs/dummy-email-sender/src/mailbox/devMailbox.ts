import { MailboxDirectory } from "./mailboxDirectory.js";
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
  /** How many messages to keep. `Infinity` keeps all of them. */
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
 * Keeps the emails sent with the Dummy email sender, so developers, tests, and
 * agents can read them.
 *
 * Messages live in memory and on disk. The disk copy survives server restarts
 * and is shared with other processes that send emails (e.g. `wasp db seed`).
 * Every read first picks up changes other processes made on disk.
 */
export class DevMailbox {
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
    this.#directory = new MailboxDirectory(options.directoryPath);
    this.#maxMessages = options.maxMessages;
    this.#getSentAt = createMonotonicClock(options.now);
  }

  /**
   * Stores the email and returns the stored message, or `undefined` when
   * `maxMessages` is `0`.
   */
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
    return this.#findMessages(filter).slice(0, limit).map(summarizeMessage);
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
   * Resolves with the newest message matching the filter. If there is none
   * yet, waits for one. Resolves with `undefined` on timeout or abort.
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

      // Subscribe before looking for an existing match, so a message that
      // arrives in between still resolves the wait.
      unsubscribe = this.subscribe((event) => {
        if (event.type === "added") {
          finishWithNewestMatch(this.#findLoadedMessages(filter));
        }
      });
      finishWithNewestMatch(this.#findMessages(filter));

      function finishWithNewestMatch(matches: DevMailboxMessage[]) {
        const newestMatch = matches[0];
        if (newestMatch !== undefined) {
          finish(newestMatch);
        }
      }
    });
  }

  /**
   * Calls the listener on every change, including changes other processes
   * make on disk. Returns a function that unsubscribes. Never calls the
   * listener before returning.
   */
  subscribe(listener: DevMailboxListener): () => void {
    this.#listeners.add(listener);
    this.#startWatchingIfNeeded();
    return () => {
      this.#listeners.delete(listener);
      this.#stopWatchingIfUnused();
    };
  }

  /** Newest first, after picking up changes on disk. */
  #findMessages(filter: DevMailboxMessageFilter): DevMailboxMessage[] {
    this.#syncWithDirectory();
    return this.#findLoadedMessages(filter);
  }

  /** Newest first, without reading the directory. */
  #findLoadedMessages(filter: DevMailboxMessageFilter): DevMailboxMessage[] {
    return [...this.#messages.values()]
      .filter((message) => matchesFilter(message, filter))
      .sort((a, b) => b.id.localeCompare(a.id));
  }

  /**
   * Applies `maxMessages` to the files on disk, then updates the messages in
   * memory to match them. Emits events only once the state is complete, so
   * listeners always see every message the sync found.
   */
  #syncWithDirectory(): void {
    const idsOnDisk = new Set(
      this.#deleteMessagesOverLimit(this.#directory.listMessageIds()),
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

  /** Takes ids sorted oldest first and returns the ones it kept. */
  #deleteMessagesOverLimit(ids: string[]): string[] {
    const excessCount = Math.max(ids.length - this.#maxMessages, 0);
    for (const id of ids.slice(0, excessCount)) {
      this.#directory.deleteMessage(id);
    }
    return ids.slice(excessCount);
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
    // File watchers can miss events (e.g. right after they start on macOS),
    // so also check the directory periodically.
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
