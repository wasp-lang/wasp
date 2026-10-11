import styles from "./DummyEmailSenderNotice.module.css";

// PRIVATE API
/**
 * Tells developers where to find the emails, since the Dummy email sender
 * prints them in the terminal instead of sending them.
 */
export function DummyEmailSenderNotice() {
  return (
    <div className={styles.notice}>
      <InfoIcon />
      <div>
        <p>You are using the Dummy email sender.</p>
        <p>
          To see sent emails, check the <code>wasp start</code> terminal.
        </p>
      </div>
    </div>
  );
}

function InfoIcon() {
  return (
    <svg
      className={styles.icon}
      xmlns="http://www.w3.org/2000/svg"
      fill="none"
      viewBox="0 0 24 24"
      strokeWidth={1.5}
      stroke="currentColor"
      aria-hidden="true"
    >
      <path
        strokeLinecap="round"
        strokeLinejoin="round"
        d="m11.25 11.25.041-.02a.75.75 0 0 1 1.063.852l-.708 2.836a.75.75 0 0 0 1.063.853l.041-.021M21 12a9 9 0 1 1-18 0 9 9 0 0 1 18 0Zm-9-3.75h.008v.008H12V8.25Z"
      />
    </svg>
  );
}
