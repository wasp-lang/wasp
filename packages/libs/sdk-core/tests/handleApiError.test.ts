import { HTTPError } from "ky";
import { describe, expect, it } from "vitest";
import { handleApiError } from "../src/browser/auth/session";
import { HttpError } from "../src/HttpError";

describe("handleApiError", () => {
  it("returns an `HttpError` with the response's status, message, and data", async () => {
    const kyHttpError = makeKyHttpError(403, {
      message: "Not allowed",
      data: { reason: "not the owner" },
    });

    const error = handleApiError(kyHttpError);

    expect(error).toBeInstanceOf(HttpError);
    expect(error).toMatchObject({
      statusCode: 403,
      message: "Not allowed",
      data: { reason: "not the owner" },
      response: kyHttpError.response,
      cause: kyHttpError,
    });
  });

  it("uses ky's message when the body isn't the one Wasp sends", () => {
    const kyHttpError = makeKyHttpError(502, "<html>Bad Gateway</html>");

    const error = handleApiError(kyHttpError);

    expect(error).toBeInstanceOf(HttpError);
    expect(error).toMatchObject({
      statusCode: 502,
      message: kyHttpError.message,
      data: undefined,
      response: kyHttpError.response,
      cause: kyHttpError,
    });
  });

  it("returns ky's error unchanged for a status an `HttpError` can't hold", () => {
    const kyHttpError = makeKyHttpError(304, undefined);

    expect(handleApiError(kyHttpError)).toBe(kyHttpError);
  });

  it("returns any other error unchanged", () => {
    const networkError = new TypeError("Failed to fetch");

    expect(handleApiError(networkError)).toBe(networkError);
  });
});

function makeKyHttpError(status: number, data: unknown): HTTPError {
  const kyHttpError = new HTTPError(
    new Response(null, { status }),
    new Request("http://localhost:3001/foo"),
    // `handleApiError` doesn't read the options.
    {} as ConstructorParameters<typeof HTTPError>[2],
  );
  // ky fills in `data` after reading the response body.
  kyHttpError.data = data;
  return kyHttpError;
}
