import type { OpenAPIV3_1 } from "openapi-types";
import { parse, type Token } from "path-to-regexp";

export type OpenApiHttpMethod =
  | "ALL"
  | "GET"
  | "POST"
  | "PUT"
  | "PATCH"
  | "DELETE"
  | "HEAD";

export interface OpenApiDocumentApi {
  name: string;
  method: OpenApiHttpMethod;
  // An Express route path, e.g. `/tasks/:id`.
  path: string;
  usesAuth: boolean;
}

export interface OpenApiDocumentConfig {
  title: string;
  version: string;
  serverUrl: string;
  apis: OpenApiDocumentApi[];
}

// `OpenAPIV3_1.HttpMethods` is an enum, so this gets its values as strings.
type OperationMethod = `${OpenAPIV3_1.HttpMethods}`;

const bearerAuthSchemeName = "bearerAuth";

const methodsMatchedByAll = [
  "get",
  "post",
  "put",
  "patch",
  "delete",
  "head",
] as const satisfies OperationMethod[];

/**
 * Describes the app's custom `api` routes as an OpenAPI 3.1 document: their
 * paths, methods, path parameters, and authentication.
 */
export function makeOpenApiDocument({
  title,
  version,
  serverUrl,
  apis,
}: OpenApiDocumentConfig): OpenAPIV3_1.Document {
  const anyApiUsesAuth = apis.some((api) => api.usesAuth);
  const paths: OpenAPIV3_1.PathsObject = {};

  for (const api of apis) {
    const openApiPaths = expressPathToOpenApiPaths(api.path);
    const methods = getOpenApiMethods(api.method);

    openApiPaths.forEach(({ path, pathParams }, pathIndex) => {
      const pathItem = (paths[path] ??= {});
      for (const method of methods) {
        // Express answers with the first matching route, so the first api
        // wins when two of them describe the same path and method.
        if (pathItem[method]) continue;

        const operationId = [
          api.name,
          ...(methods.length > 1 ? [method] : []),
          ...(openApiPaths.length > 1 ? [String(pathIndex + 1)] : []),
        ].join("_");

        pathItem[method] = {
          operationId,
          responses: {
            default: { description: "Response from the API's handler." },
          },
          ...(pathParams.length > 0 && {
            parameters: pathParams.map(makePathParameter),
          }),
          ...(anyApiUsesAuth && { security: getOperationSecurity(api) }),
        };
      }
    });
  }

  return {
    openapi: "3.1.0",
    info: { title, version },
    servers: [{ url: serverUrl }],
    paths,
    ...(anyApiUsesAuth && {
      components: {
        securitySchemes: {
          [bearerAuthSchemeName]: {
            type: "http",
            scheme: "bearer",
            description: "The session ID of a logged-in user.",
          },
        },
      },
    }),
  };
}

// Authentication is optional: Wasp sets `context.user` when the request has a
// valid session, and the handler decides what to do. Apis without auth list no
// requirements, so they don't look like we forgot them.
function getOperationSecurity(
  api: OpenApiDocumentApi,
): OpenAPIV3_1.SecurityRequirementObject[] {
  return api.usesAuth ? [{ [bearerAuthSchemeName]: [] }, {}] : [];
}

function getOpenApiMethods(
  method: OpenApiHttpMethod,
): readonly OperationMethod[] {
  return method === "ALL"
    ? methodsMatchedByAll
    : [method.toLowerCase() as Lowercase<typeof method>];
}

function makePathParameter(name: string): OpenAPIV3_1.ParameterObject {
  return { name, in: "path", required: true, schema: { type: "string" } };
}

/**
 * Converts an Express route path to OpenAPI path templates, together with the
 * names of their path parameters. Express paths with optional groups (e.g.
 * `/users{/:id}`) describe several OpenAPI paths, one per combination of the
 * groups being present or absent.
 *
 * OpenAPI has no syntax for wildcards (`*name`), so they become regular path
 * parameters.
 */
export function expressPathToOpenApiPaths(
  expressPath: string,
): { path: string; pathParams: string[] }[] {
  return expandOptionalGroups(parse(expressPath).tokens).map((tokens) => ({
    path: tokens.map(renderToken).join("") || "/",
    pathParams: [
      ...new Set(tokens.flatMap((token) => getParamName(token) ?? [])),
    ],
  }));
}

type ExpandedToken = Exclude<Token, { type: "group" }>;

function expandOptionalGroups(tokens: Token[]): ExpandedToken[][] {
  const [token, ...rest] = tokens;
  if (!token) return [[]];

  const restExpansions = expandOptionalGroups(rest);
  const tokenExpansions: ExpandedToken[][] =
    token.type === "group"
      ? [[], ...expandOptionalGroups(token.tokens)]
      : [[token]];

  return tokenExpansions.flatMap((tokenExpansion) =>
    restExpansions.map((restExpansion) => [
      ...tokenExpansion,
      ...restExpansion,
    ]),
  );
}

function renderToken(token: ExpandedToken): string {
  return token.type === "text" ? token.value : `{${token.name}}`;
}

function getParamName(token: ExpandedToken): string | undefined {
  return token.type === "text" ? undefined : token.name;
}
