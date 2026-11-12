import {
  createEmail,
  summarizeEmail,
  type DevMailboxEmail,
  type DevMailboxEmailSummary,
  type NewDevMailboxEmail,
} from "./email.js";
import { matchesFilter, type DevMailboxEmailFilter } from "./emailFilter.js";
import { createMonotonicClock, isEmailId } from "./emailId.js";
import {
  JsonFileMailboxDirectory,
  type MailboxDirectory,
} from "./mailboxDirectory.js";

export type DevMailboxOptions = {
  directoryPath: string;
  /** `Infinity` keeps all emails. */
  maxEmails: number;
};

export type DevMailboxEvent =
  | { type: "added"; email: DevMailboxEmailSummary }
  /** Also fires when an email falls outside the newest `maxEmails`. */
  | { type: "deleted"; id: string }
  | { type: "cleared" };

export type DevMailboxListener = (event: DevMailboxEvent) => void;

export type WaitForEmailOptions = {
  timeoutMs: number;
  signal?: AbortSignal;
};

const WATCH_DEBOUNCE_MS = 20;
const POLL_INTERVAL_MS = 500;

/**
 * Emails live in memory and in a directory. The directory survives restarts
 * and other processes write to it too, so every read syncs with it first.
 */
export interface DevMailbox {
  readonly directoryPath: string;
  add(newEmail: NewDevMailboxEmail): DevMailboxEmail | undefined;
  /** Newest first. */
  list(
    filter?: DevMailboxEmailFilter,
    limit?: number,
  ): DevMailboxEmailSummary[];
  get(id: string): DevMailboxEmail | undefined;
  /** Returns whether the email existed. */
  delete(id: string): boolean;
  clear(): void;
  /**
   * Resolves with the newest match, waiting for one if there is none yet.
   * Resolves with `undefined` on timeout or abort.
   */
  waitFor(
    filter: DevMailboxEmailFilter,
    options: WaitForEmailOptions,
  ): Promise<DevMailboxEmail | undefined>;
  /**
   * Includes changes other processes make. Never calls the listener before
   * returning. Returns a function that unsubscribes.
   */
  subscribe(listener: DevMailboxListener): () => void;
}

/** Deletes the oldest emails over `maxEmails` already in the directory. */
export function createDevMailbox(options: DevMailboxOptions): DevMailbox {
  return new DirectorySyncedDevMailbox(options);
}

class DirectorySyncedDevMailbox implements DevMailbox {
  readonly directoryPath: string;
  readonly #maxEmails: number;
  readonly #directory: MailboxDirectory;
  readonly #getSentAt: () => Date;
  readonly #emails = new Map<string, DevMailboxEmail>();
  readonly #listeners = new Set<DevMailboxListener>();
  #stopWatching: (() => void) | undefined;
  #pendingSync: ReturnType<typeof setTimeout> | undefined;

  constructor(options: DevMailboxOptions) {
    ensureValidMaxEmails(options.maxEmails);
    this.directoryPath = options.directoryPath;
    this.#directory = new JsonFileMailboxDirectory(options.directoryPath);
    this.#maxEmails = options.maxEmails;
    this.#getSentAt = createMonotonicClock();
    this.#deleteOldestOverLimit();
  }

  add(newEmail: NewDevMailboxEmail): DevMailboxEmail | undefined {
    if (this.#maxEmails === 0) {
      return undefined;
    }
    const sentAt = this.#getSentAt();
    const email = createEmail(newEmail, sentAt);
    this.#directory.writeEmail(email);
    this.#deleteOldestOverLimit();
    this.#syncWithDirectory();
    return email;
  }

  list(
    filter: DevMailboxEmailFilter = {},
    limit: number = Infinity,
  ): DevMailboxEmailSummary[] {
    return this.#syncAndFindNewestFirst(filter)
      .slice(0, limit)
      .map(summarizeEmail);
  }

  get(id: string): DevMailboxEmail | undefined {
    this.#syncWithDirectory();
    return this.#emails.get(id);
  }

  delete(id: string): boolean {
    if (!isEmailId(id) || this.get(id) === undefined) {
      return false;
    }
    this.#directory.deleteEmail(id);
    this.#syncWithDirectory();
    return true;
  }

  clear(): void {
    for (const id of this.#directory.listEmailIds()) {
      this.#directory.deleteEmail(id);
    }
    this.#emails.clear();
    this.#emit({ type: "cleared" });
  }

  waitFor(
    filter: DevMailboxEmailFilter,
    options: WaitForEmailOptions,
  ): Promise<DevMailboxEmail | undefined> {
    return new Promise((resolve) => {
      let isSettled = false;
      let unsubscribe: (() => void) | undefined;
      const finish = (email: DevMailboxEmail | undefined) => {
        if (isSettled) {
          return;
        }
        isSettled = true;
        clearTimeout(timeout);
        options.signal?.removeEventListener("abort", onAbort);
        unsubscribe?.();
        resolve(email);
      };
      const onAbort = () => finish(undefined);
      const timeout = setTimeout(() => finish(undefined), options.timeoutMs);
      options.signal?.addEventListener("abort", onAbort);
      if (options.signal?.aborted) {
        finish(undefined);
        return;
      }

      // Subscribing first means an email arriving before the lookup isn't missed.
      unsubscribe = this.subscribe((event) => {
        if (event.type === "added") {
          finishWithNewestMatch(this.#findLoadedNewestFirst(filter));
        }
      });
      finishWithNewestMatch(this.#syncAndFindNewestFirst(filter));

      function finishWithNewestMatch(matches: DevMailboxEmail[]) {
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

  #syncAndFindNewestFirst(filter: DevMailboxEmailFilter): DevMailboxEmail[] {
    this.#syncWithDirectory();
    return this.#findLoadedNewestFirst(filter);
  }

  #findLoadedNewestFirst(filter: DevMailboxEmailFilter): DevMailboxEmail[] {
    return [...this.#emails.values()]
      .filter((email) => matchesFilter(email, filter))
      .sort((a, b) => b.id.localeCompare(a.id));
  }

  /**
   * Never deletes files. Loads only the newest `maxEmails` emails. Emits events
   * only after the whole sync, so listeners see every new email.
   */
  #syncWithDirectory(): void {
    const { withinLimit } = this.#splitAtLimit(this.#directory.listEmailIds());
    const idsInMailbox = new Set(withinLimit);

    const deletedIds = [...this.#emails.keys()].filter(
      (id) => !idsInMailbox.has(id),
    );
    for (const id of deletedIds) {
      this.#emails.delete(id);
    }

    const addedEmails = [...idsInMailbox]
      .filter((id) => !this.#emails.has(id))
      .sort()
      .map((id) => this.#directory.readEmail(id))
      .filter((email) => email !== undefined);
    for (const email of addedEmails) {
      this.#emails.set(email.id, email);
    }

    for (const id of deletedIds) {
      this.#emit({ type: "deleted", id });
    }
    for (const email of addedEmails) {
      this.#emit({ type: "added", email: summarizeEmail(email) });
    }
  }

  #deleteOldestOverLimit(): void {
    const { overLimit } = this.#splitAtLimit(this.#directory.listEmailIds());
    for (const id of overLimit) {
      this.#directory.deleteEmail(id);
    }
  }

  #splitAtLimit(idsOldestFirst: string[]): {
    overLimit: string[];
    withinLimit: string[];
  } {
    const excessCount = Math.max(idsOldestFirst.length - this.#maxEmails, 0);
    return {
      overLimit: idsOldestFirst.slice(0, excessCount),
      withinLimit: idsOldestFirst.slice(excessCount),
    };
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

function ensureValidMaxEmails(maxEmails: number): void {
  const isValid =
    maxEmails === Infinity || (Number.isInteger(maxEmails) && maxEmails >= 0);
  if (!isValid) {
    throw new Error(
      `Dev mailbox maxEmails must be a non-negative integer or Infinity, got ${maxEmails}.`,
    );
  }
}
