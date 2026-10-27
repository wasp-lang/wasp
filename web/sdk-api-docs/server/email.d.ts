/**
 * Send emails from your server code with the provider you set up in the
 * `emailSender` field of the app spec.
 *
 * @example
 * ```ts
 * import { emailSender } from "wasp/server/email";
 *
 * // In some action handler...
 * const info = await emailSender.send({
 *   from: {
 *     name: "John Doe",
 *     email: "john@doe.com",
 *   },
 *   to: "user@domain.com",
 *   subject: "Saying hello",
 *   text: "Hello world",
 *   html: "Hello <strong>world</strong>",
 * });
 * ```
 *
 * @see [Sending emails](https://wasp.sh/docs/features/email)
 *
 * @module
 */

import type {
  EmailFromField,
  SentMessageInfo,
} from "@wasp.sh/lib-sdk-core/node";

/**
 * Sends emails with the provider you set up in the `emailSender` field of the
 * app spec.
 *
 * @example
 * ```ts
 * import { emailSender } from "wasp/server/email";
 *
 * const info = await emailSender.send({
 *   to: "user@domain.com",
 *   subject: "Saying hello",
 *   text: "Hello world",
 *   html: "Hello <strong>world</strong>",
 * });
 * ```
 *
 * @see [Sending emails](https://wasp.sh/docs/features/email)
 * @see [Providers](https://wasp.sh/docs/features/email#providers)
 */
export declare const emailSender: EmailSender;

/**
 * The type of {@link emailSender}.
 */
export type EmailSender = {
  /**
   * Sends an email.
   *
   * @param email The email to send.
   * @returns The status of the sent email. It varies depending on the
   *   provider you use.
   */
  send(email: Email): Promise<SentMessageInfo>;
};

/**
 * An email to send with {@link EmailSender.send | `emailSender.send`}.
 */
export type Email = {
  /**
   * The sender's details.
   *
   * It's optional if you set up the `defaultFrom` field of `emailSender` in
   * the app spec, and required otherwise.
   */
  from?: EmailFromField;
  /** The recipient's email address. */
  to: string;
  /** The subject of the email. */
  subject: string;
  /** The text version of the email. */
  text: string;
  /** The HTML version of the email. */
  html: string;
};

/**
 * The sender of an {@link Email}.
 *
 * It has the sender's `email` address, and optionally their `name`.
 */
export type { EmailFromField } from "@wasp.sh/lib-sdk-core/node";

/**
 * The status of a sent email, returned by
 * {@link EmailSender.send | `emailSender.send`}. It varies depending on the
 * provider you use.
 */
export type { SentMessageInfo } from "@wasp.sh/lib-sdk-core/node";
