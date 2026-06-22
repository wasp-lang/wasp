/**
 * Used purely to help compiler check for exhaustiveness in switch statements,
 * will never execute. See https://stackoverflow.com/a/39419171.
 */
export function assertUnreachable(
  // eslint-disable-next-line @typescript-eslint/no-unused-vars
  _: never,
): never {
  throw Error("This code should be unreachable");
}

/**
 * Allows for throttling a function call while still allowing the last invocation to be executed after the throttle delay ends.
 */
export function throttleWithTrailingInvocation(
  fn: () => void,
  delayInMilliseconds: number,
): (() => void) & { cancel: () => void } {
  let fnLastCallTime: number | null = null;
  let trailingInvocationTimeoutId: ReturnType<typeof setTimeout> | null = null;
  let isTrailingInvocationPending = false;

  const callFn = () => {
    fnLastCallTime = Date.now();
    fn();
  };

  const throttledFn = () => {
    const currentTime = Date.now();
    const timeSinceLastExecution = fnLastCallTime
      ? currentTime - fnLastCallTime
      : 0;

    const shouldCallImmediately =
      fnLastCallTime === null || timeSinceLastExecution >= delayInMilliseconds;

    if (shouldCallImmediately) {
      callFn();
      return;
    }

    if (!isTrailingInvocationPending) {
      isTrailingInvocationPending = true;
      const remainingDelayTime = Math.max(
        delayInMilliseconds - timeSinceLastExecution,
        0,
      );

      trailingInvocationTimeoutId = setTimeout(() => {
        callFn();
        isTrailingInvocationPending = false;
      }, remainingDelayTime);
    }
  };

  throttledFn.cancel = () => {
    if (trailingInvocationTimeoutId) {
      clearTimeout(trailingInvocationTimeoutId);
      trailingInvocationTimeoutId = null;
    }
    isTrailingInvocationPending = false;
  };

  return throttledFn as typeof throttledFn & { cancel: () => void };
}
