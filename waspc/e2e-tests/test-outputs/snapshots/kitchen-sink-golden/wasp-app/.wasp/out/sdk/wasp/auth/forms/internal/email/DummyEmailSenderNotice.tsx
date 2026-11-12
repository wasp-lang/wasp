import { config } from '../../../../client/index.js'
import styles from "./DummyEmailSenderNotice.module.css";

const devMailboxUrl = `${config.apiUrl}/__wasp/mailbox`;

// PRIVATE API
/**
 * Tells developers where to find the emails, since the Dummy email sender
 * keeps them in the dev mailbox instead of sending them.
 */
export function DummyEmailSenderNotice() {
  return (
    <div className={styles.notice}>
      <p>You are using the Dummy email sender.</p>
      <p>
        To see sent emails, open the{" "}
        <a href={devMailboxUrl} target="_blank" rel="noreferrer">
          dev mailbox
        </a>
        .
      </p>
    </div>
  );
}
