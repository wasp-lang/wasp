import type { OnAfterLoginHook, OnBeforeSignupHook } from "wasp/server/auth";
/**
 * App-level lifecycle hooks (`auth.hooks` in main.wasp.ts).
 *
 * These fire at Wasp-owned choke points -- identity provisioning and session
 * minting -- so they cover the `@wasp.sh/auth` PACKAGE's flows too: the
 * adapter never calls them, Wasp does, which is what makes the veto below
 * impossible for a provider to skip.
 */
export declare const onBeforeSignup: OnBeforeSignupHook;
export declare const onAfterLogin: OnAfterLoginHook;
