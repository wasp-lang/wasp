/**
 * Types for implementing your app's
 * [APIs](https://wasp.sh/docs/features/apis) (custom HTTP endpoints).
 *
 * Wasp generates one generic type per `api` you declare in the app spec, named
 * after its implementation function in PascalCase (e.g. `FooBar` for
 * `fooBar`). It's an {@link AuthenticatedApi} if the API uses auth, or an
 * {@link Api} otherwise. An API uses auth if your app uses auth, unless you set
 * `auth: false` in its spec.
 *
 * To make sure Wasp generates the types, add your `api`s to the app spec
 * first, and keep `wasp start` running.
 *
 * @example
 * ```ts
 * import type { FooBar } from "wasp/server/api";
 *
 * export const fooBar: FooBar = (req, res, context) => {
 *   // Example of modifying headers to override Wasp default CORS middleware.
 *   res.set("Access-Control-Allow-Origin", "*");
 *   res.json({
 *     msg: `Hello, ${context.user ? "registered user" : "stranger"}!`,
 *   });
 * };
 * ```
 *
 * @see [API routes](https://wasp.sh/docs/features/apis)
 *
 * @module
 */

import type { Request, Response } from "express";
import type {
  Query as ExpressQuery,
  ParamsDictionary,
} from "express-serve-static-core";
import type { AuthUser } from "../auth";

/**
 * The type of an API's implementation, when the API uses auth.
 *
 * The implementation is a Node.js function that takes three arguments:
 *
 * 1. `req`: The Express `Request` object.
 * 2. `res`: The Express `Response` object. Use it to send the response.
 * 3. `context`: An additional context object **injected into the API by
 *    Wasp**. It contains the logged-in `user`, and the Entities the API uses
 *    in `entities`.
 *
 * Wasp generates one type per API, named after its implementation function
 * in PascalCase (e.g. `FooBar` for `fooBar`), that fills in `Entities` with
 * the Entities from the API's spec. You can pass it the other type arguments
 * to type the request and response (e.g. `FooBar<{ email: string }, { answer:
 * number }>` for the params and the response body).
 *
 * @typeParam Entities The type of `context.entities`: an object with the
 *   Prisma API for each Entity in the API's spec (e.g. `{ Task: ... }` for
 *   `entities: ["Task"]`). The generated type fills it in for you.
 * @typeParam Params The type of the route params in `req.params`.
 * @typeParam ResBody The type of the response body.
 * @typeParam ReqBody The type of the request body in `req.body`.
 * @typeParam ReqQuery The type of the query string params in `req.query`.
 * @typeParam Locals The type of `res.locals`.
 *
 * @example
 * With the API declared as
 * `api("GET", "/foo/bar/:email", fooBar, { entities: ["Task"] })`:
 *
 * ```ts
 * import type { FooBar } from "wasp/server/api";
 *
 * export const fooBar: FooBar<
 *   { email: string }, // params
 *   { answer: number } // response
 * > = (req, res, _context) => {
 *   console.log(req.params.email);
 *   res.json({ answer: 42 });
 * };
 * ```
 *
 * @example
 * Using Entities:
 *
 * ```ts
 * import type { FooBar } from "wasp/server/api";
 *
 * export const fooBar: FooBar = async (req, res, context) => {
 *   res.json({ count: await context.entities.Task.count() });
 * };
 * ```
 *
 * @see [Defining the API's Node.js implementation](https://wasp.sh/docs/features/apis#defining-the-apis-nodejs-implementation)
 * @see [Using Entities in APIs](https://wasp.sh/docs/features/apis#using-entities-in-apis)
 */
export type AuthenticatedApi<
  Entities,
  Params extends ParamsDictionary = ParamsDictionary,
  ResBody = any,
  ReqBody = any,
  ReqQuery extends ExpressQuery = ExpressQuery,
  Locals extends Record<string, any> = Record<string, any>,
> = (
  req: Request<Params, ResBody, ReqBody, ReqQuery, Locals>,
  res: Response<ResBody, Locals>,
  context: AuthenticatedApiContext<Entities>,
) => void;

/**
 * The type of an API's implementation, when the API doesn't use auth.
 *
 * It's the same as {@link AuthenticatedApi}, except that the `context`
 * doesn't include the `user`. Wasp uses it for the generated API types (e.g.
 * `FooBar`) when your app doesn't use auth, or when you set `auth: false` in
 * the API's spec.
 *
 * @typeParam Entities The type of `context.entities`: an object with the
 *   Prisma API for each Entity in the API's spec (e.g. `{ Task: ... }` for
 *   `entities: ["Task"]`). The generated type fills it in for you.
 * @typeParam Params The type of the route params in `req.params`.
 * @typeParam ResBody The type of the response body.
 * @typeParam ReqBody The type of the request body in `req.body`.
 * @typeParam ReqQuery The type of the query string params in `req.query`.
 * @typeParam Locals The type of `res.locals`.
 *
 * @see [Defining the API's Node.js implementation](https://wasp.sh/docs/features/apis#defining-the-apis-nodejs-implementation)
 */
export type Api<
  Entities,
  Params extends ParamsDictionary = ParamsDictionary,
  ResBody = any,
  ReqBody = any,
  ReqQuery extends ExpressQuery = ExpressQuery,
  Locals extends Record<string, any> = Record<string, any>,
> = (
  req: Request<Params, ResBody, ReqBody, ReqQuery, Locals>,
  res: Response<ResBody, Locals>,
  context: ApiContext<Entities>,
) => void;

/**
 * The `context` Wasp passes to the implementation of an API that doesn't use
 * auth.
 */
type ApiContext<Entities> = {
  /**
   * The Prisma API for each Entity in the API's spec, keyed by the Entity's
   * name. For example, `context.entities.Task` exposes `prisma.task` from
   * [Prisma's CRUD API](https://www.prisma.io/docs/orm/prisma-client/queries/crud).
   *
   * @see [Using Entities in APIs](https://wasp.sh/docs/features/apis#using-entities-in-apis)
   */
  entities: Entities;
};

/**
 * The `context` Wasp passes to the implementation of an API that uses auth.
 */
type AuthenticatedApiContext<Entities> = {
  /**
   * The Prisma API for each Entity in the API's spec, keyed by the Entity's
   * name. For example, `context.entities.Task` exposes `prisma.task` from
   * [Prisma's CRUD API](https://www.prisma.io/docs/orm/prisma-client/queries/crud).
   *
   * @see [Using Entities in APIs](https://wasp.sh/docs/features/apis#using-entities-in-apis)
   */
  entities: Entities;
  /**
   * The logged-in user, or `undefined` if the request isn't authenticated.
   *
   * @see [Using the `context.user` object](https://wasp.sh/docs/features/auth/overview#using-the-contextuser-object)
   */
  user?: AuthUser;
};

export {};
