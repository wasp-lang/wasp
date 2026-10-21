import styles from "./DummyEmailSenderNotice.module.css";

/**
 * Tells developers where to find the emails, since the Dummy email sender
 * prints them in the terminal instead of sending them.
 */
export function DummyEmailSenderNotice() {
  return (
    <div className={styles.notice}>
      <p>You are using the Dummy email sender.</p>
      <p>
        To see sent emails, check the <code>wasp start</code> terminal.
      </p>
    </div>
  );
}
