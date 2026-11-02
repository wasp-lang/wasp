import {
  render,
  type RenderOptions,
  type RenderResult,
} from "@testing-library/react";
import type { ReactElement } from "react";
import { WaspTestWrapper } from "./WaspTestWrapper.js";

/**
 * Renders a component inside the providers that Wasp's client code needs
 * (React Query and React Router).
 *
 * @see https://testing-library.com/docs/react-testing-library/api#render
 */
export function renderInContext(
  ui: ReactElement,
  options?: Omit<RenderOptions, "wrapper">,
): RenderResult {
  return render(ui, { ...options, wrapper: WaspTestWrapper });
}
