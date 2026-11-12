import * as vm from "node:vm";

import { describe, expect, it } from "vitest";

import { makeInboxPage } from "../src/mailbox/inboxPage";

function makePage(): string {
  return makeInboxPage({
    appName: "My <App>",
    apiBasePath: "/__wasp/mailbox/api",
    directoryPath: ".wasp/mailbox/",
  });
}

describe("makeInboxPage", () => {
  it("has an inline script that parses", () => {
    const script = /<script>([\s\S]*)<\/script>/.exec(makePage())?.[1];

    expect(script).toBeDefined();
    expect(() => new vm.Script(script!)).not.toThrow();
  });

  it("escapes the app name", () => {
    expect(makePage()).toContain("My &lt;App&gt;");
  });
});
