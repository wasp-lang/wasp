import { useEffect, useState } from "react";

/**
 * The dashboard's origin (`https://linkly.example`), or "" until the page has
 * hydrated. Prerendered pages have no `window` at build time, so they render
 * root-relative links first and switch to absolute ones in the browser. Reading
 * `window` during render instead would make the prerendered HTML and the first
 * client render differ, and React would throw the prerendered HTML away.
 */
export function useOrigin(): string {
  const [origin, setOrigin] = useState("");
  useEffect(() => setOrigin(window.location.origin), []);
  return origin;
}
