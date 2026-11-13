import { describe, expect, it } from "vitest";
import {
  expressPathToOpenApiPaths,
  makeOpenApiDocument,
  type OpenApiDocumentApi,
} from "../../src/node/openApi";

describe("expressPathToOpenApiPaths", () => {
  it("keeps static paths as they are", () => {
    expect(expressPathToOpenApiPaths("/foo/bar")).toEqual([
      { path: "/foo/bar", pathParams: [] },
    ]);
  });

  it("converts params and wildcards to path parameters", () => {
    expect(expressPathToOpenApiPaths("/users/:userId/files/*filePath")).toEqual(
      [
        {
          path: "/users/{userId}/files/{filePath}",
          pathParams: ["userId", "filePath"],
        },
      ],
    );
  });

  it("supports quoted param names", () => {
    expect(expressPathToOpenApiPaths('/users/:"user id"')).toEqual([
      { path: "/users/{user id}", pathParams: ["user id"] },
    ]);
  });

  it("keeps escaped characters as literals", () => {
    expect(expressPathToOpenApiPaths("/foo\\:bar")).toEqual([
      { path: "/foo:bar", pathParams: [] },
    ]);
  });

  it("expands optional groups into one path per combination", () => {
    expect(expressPathToOpenApiPaths("/users{/:id{/edit}}")).toEqual([
      { path: "/users", pathParams: [] },
      { path: "/users/{id}", pathParams: ["id"] },
      { path: "/users/{id}/edit", pathParams: ["id"] },
    ]);
  });

  it("returns the root path for an empty path", () => {
    expect(expressPathToOpenApiPaths("{/:lang}")).toEqual([
      { path: "/", pathParams: [] },
      { path: "/{lang}", pathParams: ["lang"] },
    ]);
  });
});

describe("makeOpenApiDocument", () => {
  const makeDocument = (apis: OpenApiDocumentApi[]) =>
    makeOpenApiDocument({
      title: "Test App",
      version: "1.2.3",
      serverUrl: "http://localhost:3001",
      apis,
    });

  const defaultResponses = {
    default: { description: "Response from the API's handler." },
  };

  const idParam = {
    name: "id",
    in: "path",
    required: true,
    schema: { type: "string" },
  };

  it("returns a document with no paths when there are no apis", () => {
    expect(makeDocument([])).toEqual({
      openapi: "3.1.0",
      info: { title: "Test App", version: "1.2.3" },
      servers: [{ url: "http://localhost:3001" }],
      paths: {},
    });
  });

  it("describes each api route as an operation", () => {
    const document = makeDocument([
      { name: "getTask", method: "GET", path: "/tasks/:id", usesAuth: false },
      {
        name: "updateTask",
        method: "PATCH",
        path: "/tasks/:id",
        usesAuth: false,
      },
      { name: "webhook", method: "ALL", path: "/webhook", usesAuth: false },
    ]);

    expect(document.paths).toEqual({
      "/tasks/{id}": {
        get: {
          operationId: "getTask",
          responses: defaultResponses,
          parameters: [idParam],
        },
        patch: {
          operationId: "updateTask",
          responses: defaultResponses,
          parameters: [idParam],
        },
      },
      "/webhook": Object.fromEntries(
        ["get", "post", "put", "patch", "delete", "head"].map((method) => [
          method,
          { operationId: `webhook_${method}`, responses: defaultResponses },
        ]),
      ),
    });
  });

  it("numbers the operations of an api with optional groups", () => {
    const document = makeDocument([
      {
        name: "getTasks",
        method: "GET",
        path: "/tasks{/:id}",
        usesAuth: false,
      },
    ]);

    expect(document.paths).toEqual({
      "/tasks": {
        get: { operationId: "getTasks_1", responses: defaultResponses },
      },
      "/tasks/{id}": {
        get: {
          operationId: "getTasks_2",
          responses: defaultResponses,
          parameters: [idParam],
        },
      },
    });
  });

  it("keeps the first api when two describe the same path and method", () => {
    const document = makeDocument([
      { name: "first", method: "GET", path: "/tasks", usesAuth: false },
      { name: "second", method: "ALL", path: "/tasks", usesAuth: false },
    ]);

    expect(document.paths?.["/tasks"]?.get?.operationId).toBe("first");
    expect(document.paths?.["/tasks"]?.post?.operationId).toBe("second_post");
  });

  it("marks apis that use auth with an optional bearer token, and the rest with no security", () => {
    const document = makeDocument([
      { name: "me", method: "GET", path: "/me", usesAuth: true },
      { name: "status", method: "GET", path: "/status", usesAuth: false },
    ]);

    expect(document.paths).toEqual({
      "/me": {
        get: {
          operationId: "me",
          responses: defaultResponses,
          security: [{ bearerAuth: [] }, {}],
        },
      },
      "/status": {
        get: {
          operationId: "status",
          responses: defaultResponses,
          security: [],
        },
      },
    });
    expect(document.components).toEqual({
      securitySchemes: {
        bearerAuth: {
          type: "http",
          scheme: "bearer",
          description: "The session ID of a logged-in user.",
        },
      },
    });
  });
});
