/**
 * Type-safe links and URLs for the
 * [routes](https://wasp.sh/docs/features/pages-and-routes) you declare in your
 * Wasp Spec.
 *
 * Wasp generates the {@link routes} object and the types of {@link Link}'s and
 * {@link NavLink}'s props from your app's routes, so linking to a route that
 * doesn't exist, or passing the wrong params, is a type error.
 *
 * @module
 */

import type { Params, Search } from "@wasp/lib-sdk-core";
import type { JSX, RefAttributes } from "react";
import type {
  LinkProps as RouterLinkProps,
  NavLinkProps as RouterNavLinkProps,
} from "react-router";

/**
 * The props that pick which route a {@link Link} or {@link NavLink} points to,
 * and how to fill in its URL.
 */
type RouteLinkProps = {
  /**
   * The path of one of the routes in your Wasp Spec, exactly as you declared
   * it, e.g. `"/task/:id"`.
   *
   * If the path has optional static segments, pass one of the paths that
   * include or exclude them. For example, if the path is `/task/:id/details?`,
   * pass either `/task/:id/details` or `/task/:id`.
   */
  to: string;
  /**
   * The values for the params in the path, e.g. `{ id: 1 }` for `/task/:id`.
   * Required if the path contains params. You can leave out optional params
   * (`:userId?`).
   *
   * For a splat path like `/pages/*`, pass the matched rest of the path as
   * `{ "*": "some/path" }`.
   */
  params?: Params;
  /**
   * The search params (query string), in any shape the
   * [`URLSearchParams`](https://developer.mozilla.org/en-US/docs/Web/API/URLSearchParams/URLSearchParams)
   * constructor accepts. For example, `{ sortBy: "date" }` becomes
   * `?sortBy=date`.
   */
  search?: Search;
  /** The hash (fragment) of the URL, e.g. `"comments"` becomes `#comments`. */
  hash?: string;
};

/**
 * The props of the {@link Link} component.
 *
 * Use this type to build your own component on top of `Link` while keeping
 * its type safety.
 *
 * @example
 * ```tsx
 * import { Link, type LinkProps } from "wasp/client/router";
 *
 * export function ButtonLink({ className, ...linkProps }: LinkProps) {
 *   return <Link {...linkProps} className={`button ${className ?? ""}`} />;
 * }
 * ```
 */
export type LinkProps = Omit<
  RouterLinkProps & RefAttributes<HTMLAnchorElement>,
  "to"
> &
  RouteLinkProps;

/**
 * A link to one of your app's routes.
 *
 * It behaves the same as
 * [React Router's `Link`](https://reactrouter.com/8.0.1/api/components/Link)
 * and accepts all of its props, but `to` must be a route path from your
 * Wasp Spec, and `params` must match the params in that path.
 *
 * @example
 * ```tsx
 * import { Link } from "wasp/client/router";
 *
 * export function PhotoCard({ photoId }: { photoId: string }) {
 *   return (
 *     <Link to="/photo/:photoId" params={{ photoId }} search={{ size: "large" }}>
 *       Photo {photoId}
 *     </Link>
 *   );
 * }
 * ```
 *
 * @see [Navigating between pages](https://wasp.sh/docs/features/pages-and-routes#link-component)
 *
 * @component
 */
export declare function Link(props: LinkProps): JSX.Element;

/**
 * The props of the {@link NavLink} component.
 *
 * Use this type to build your own component on top of `NavLink` while keeping
 * its type safety.
 */
export type NavLinkProps = Omit<
  RouterNavLinkProps & RefAttributes<HTMLAnchorElement>,
  "to"
> &
  RouteLinkProps;

/**
 * A link to one of your app's routes that knows whether it's active or
 * pending.
 *
 * It behaves the same as
 * [React Router's `NavLink`](https://reactrouter.com/8.0.1/api/components/NavLink)
 * and accepts all of its props, with the same `to`, `params`, `search`, and
 * `hash` props as {@link Link}. Use it when the current page should be
 * highlighted, or to show a spinner during a pending navigation.
 *
 * The most useful React Router props are:
 *
 * - `className`, `style`, and `children`: besides regular values, these can be
 *   functions. `NavLink` calls them with an object describing the link's state
 *   and uses what they return. The object has these fields:
 *   - `isActive`: the link points to the current page.
 *   - `isPending`: the user clicked the link, and the page is still loading.
 *   - `isTransitioning`: a
 *     [view transition](https://reactrouter.com/8.0.1/how-to/view-transitions)
 *     to the page is in progress.
 * - `end: boolean`: by default, a link is also active on nested paths. For
 *   example, a link to `/tasks` is active on `/tasks/123`. Set `end` to only
 *   make it active on its exact path.
 * - `caseSensitive: boolean`: by default, the link ignores letter case when
 *   checking if it's active. Set `caseSensitive` to take case into account.
 *
 * @example
 * ```tsx
 * import { NavLink } from "wasp/client/router";
 *
 * export function Navigation() {
 *   return (
 *     <nav>
 *       <NavLink
 *         to="/tasks"
 *         className={({ isActive }) =>
 *           isActive ? "font-bold text-blue-600" : "text-gray-600"
 *         }
 *       >
 *         Tasks
 *       </NavLink>
 *     </nav>
 *   );
 * }
 * ```
 *
 * @see [Reacting to navigation state with `NavLink`](https://wasp.sh/docs/features/pages-and-routes#reacting-to-navigation-state-with-navlink)
 *
 * @component
 */
export declare function NavLink(props: NavLinkProps): JSX.Element;

/**
 * The options for building the URL of a route with {@link RouteEntry.build}.
 */
type RouteBuildOptions = {
  /**
   * The values for the params in the route's path, in the same form as
   * {@link RouteLinkProps.params}. Required if the path contains params.
   */
  params?: Params;
  /**
   * Which variant of the path to build. Required (and only accepted) if the
   * route's path has optional static segments. See {@link RouteLinkProps.to}
   * for the values it takes.
   */
  path?: string;
  /**
   * The search params (query string), in the same shapes as
   * {@link RouteLinkProps.search}.
   */
  search?: Search;
  /** The hash (fragment) of the URL. */
  hash?: string;
};

/**
 * The entry that Wasp generates in {@link routes} for each route in your Wasp
 * Spec.
 */
type RouteEntry = {
  /** The route's path, as declared in your Wasp Spec, e.g. `"/task/:id"`. */
  readonly to: string;
  /**
   * Builds the URL of the route, filling in its params, search params, and
   * hash.
   *
   * `options` is optional only if the route's path has no params and no
   * optional static segments.
   *
   * @returns The URL, e.g. `"/task/42?tab=comments#notes"`.
   */
  build(options?: RouteBuildOptions): string;
};

/**
 * One {@link RouteEntry} for each route in your Wasp Spec, under the route's
 * name.
 *
 * Use it to build a route's URL from your own code, for example to navigate
 * after a form submits.
 *
 * For routes declared like this:
 *
 * ```ts
 * route("RootRoute", "/", page(MainPage)),
 * route("DetailRoute", "/task/:id/:userId?", page(DetailPage)),
 * route("OptionalRoute", "/task/:id/details?", page(TaskPage)),
 * route("CatchAllRoute", "/pages/*", page(CatchAllPage)),
 * ```
 *
 * Wasp generates `routes` with these `build` functions:
 *
 * ```ts
 * routes.RootRoute.build(options?: {
 *   search?: Search;
 *   hash?: string;
 * });
 *
 * routes.DetailRoute.build(options: {
 *   params: { id: ParamValue; userId?: ParamValue };
 *   search?: Search;
 *   hash?: string;
 * });
 *
 * routes.OptionalRoute.build(options: {
 *   path: "/task/:id/details" | "/task/:id";
 *   params: { id: ParamValue };
 *   search?: Search;
 *   hash?: string;
 * });
 *
 * routes.CatchAllRoute.build(options: {
 *   params: { "*": ParamValue };
 *   search?: Search;
 *   hash?: string;
 * });
 * ```
 *
 * @example
 * ```tsx
 * import { useNavigate } from "react-router";
 * import { routes } from "wasp/client/router";
 *
 * export function NewPhotoForm() {
 *   const navigate = useNavigate();
 *
 *   async function handleSubmit() {
 *     const photoId = await uploadPhoto(); // your upload logic
 *     navigate(routes.PhotoRoute.build({ params: { photoId } }));
 *   }
 *
 *   // ...
 * }
 * ```
 *
 * @see [Navigating between pages](https://wasp.sh/docs/features/pages-and-routes#link-component)
 */
export declare const routes: { readonly [routeName: string]: RouteEntry };

// Only the declarations marked with `export` are exported.
export {};
