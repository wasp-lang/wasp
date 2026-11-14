import { afterEach, beforeEach, describe, expect, it } from "vitest";

import { startMailboxServer, type MailboxServer } from "../mailboxServer";

let mailboxServer: MailboxServer;

beforeEach(async () => {
  mailboxServer = await startMailboxServer({ appName: "My <App>" });
});

afterEach(() => mailboxServer.close());

describe("inbox page", () => {
  it("fills in and escapes the app name and storage path", async () => {
    const response = await fetch(`${mailboxServer.mailboxUrl}/`);

    expect(response.headers.get("content-type")).toMatch(/text\/html/);
    const page = await response.text();
    expect(page).toContain("My &lt;App&gt;");
    expect(page).toContain(mailboxServer.mailbox.directoryPath);
    expect(page).not.toMatch(/%[A-Z_]+%/);
  });

  it("redirects to the trailing slash its relative URLs need", async () => {
    const response = await fetch(`${mailboxServer.mailboxUrl}?a=1`, {
      redirect: "manual",
    });

    expect(response.status).toBe(302);
    expect(response.headers.get("location")).toBe("/__wasp/mailbox/?a=1");
  });

  it("serves its stylesheet", async () => {
    const response = await fetch(`${mailboxServer.mailboxUrl}/inbox.css`);

    expect(response.headers.get("content-type")).toMatch(/text\/css/);
  });
});
