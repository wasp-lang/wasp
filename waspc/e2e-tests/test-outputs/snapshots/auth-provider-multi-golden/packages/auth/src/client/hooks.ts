import { useEffect, useRef } from "react";

/** Runs the effect once even under React StrictMode's double render. */
export function useEffectOnce(callback: () => void) {
  const hasRun = useRef(false);
  useEffect(() => {
    if (!hasRun.current) {
      callback();
      hasRun.current = true;
    }
  }, [callback]);
}
