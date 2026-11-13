import { expect, test } from "@playwright/test";
import { WASP_SERVER_URL } from "../playwright.config";

test.describe("OpenAPI document", () => {
  test("the server describes the custom APIs", async ({ request }) => {
    const response = await request.get(`${WASP_SERVER_URL}/openapi.json`);

    expect(response.ok()).toBe(true);
    const document = await response.json();

    expect(document.openapi).toBe("3.1.0");
    expect(document.info.title).toBe("Wasp Kitchen Sink");
    expect(document.servers).toEqual([{ url: expect.any(String) }]);
    expect(Object.keys(document.paths["/bar/baz"]).sort()).toEqual([
      "get",
      "head",
      "patch",
    ]);
    expect(document.paths["/bar/baz"].get.operationId).toBe("barBaz");
    expect(document.paths["/bar/baz"].get.security).toEqual([]);
    expect(document.paths["/foo/bar"].post).toMatchObject({
      operationId: "fooBar_post",
      security: [{ bearerAuth: [] }, {}],
    });
  });
});
