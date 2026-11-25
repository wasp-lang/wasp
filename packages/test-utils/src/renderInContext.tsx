import {
  render,
  type RenderOptions,
  type RenderResult,
} from "@testing-library/react";
import type { ReactElement } from "react";
import { WaspTestWrapper } from "./WaspTestWrapper.js";

/**
 * Renders a React element inside the providers that Wasp's client code needs,
 * _react-query_'s `QueryClientProvider` and React Router's `BrowserRouter`.
 *
 * Use it instead of Testing Library's `render` in your component tests, so your
 * components can use `useQuery`, `useAction`, and links. Each call gets its own
 * query client, so tests don't share cached data.
 *
 * It wraps the element in {@link WaspTestWrapper}.
 *
 * @param ui The element to render.
 * @param options The options of Testing Library's
 * [`render`](https://testing-library.com/docs/react-testing-library/api#render-options),
 * except `wrapper`.
 * @returns What Testing Library's
 * [`render`](https://testing-library.com/docs/react-testing-library/api#render-result)
 * returns.
 *
 * @example
 * ```tsx
 * import { screen } from "@testing-library/react";
 * import { renderInContext } from "@wasp/test-utils";
 * import { expect, test } from "vitest";
 * import { TasksHeader } from "./TasksHeader";
 *
 * test("links back home", () => {
 *   renderInContext(<TasksHeader />);
 *
 *   expect(screen.getByRole("link", { name: "Back home" })).toBeInTheDocument();
 * });
 * ```
 */
export function renderInContext(
  ui: ReactElement,
  options?: Omit<RenderOptions, "wrapper">,
): RenderResult {
  return render(ui, { ...options, wrapper: WaspTestWrapper });
}
