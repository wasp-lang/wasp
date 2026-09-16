import { prisma } from '../index.js';
import { onBeforeSignup as onBeforeSignupHook_ext } from 'virtual:wasp/user/auth/hooks';
import { onAfterLogin as onAfterLoginHook_ext } from 'virtual:wasp/user/auth/hooks';
/**
 * Runs a veto-able hook (onBeforeSignup, onBeforeLogin) and tags whatever it
 * throws with the contract's `wasp-auth/policy-veto` code -- tagging, not
 * wrapping, so the error's type, message and any `statusCode` survive for
 * Wasp's own error handling, while an adapter package (which only speaks
 * contract codes) can map the rejection to a 4xx instead of a 500. An error
 * that already carries a code keeps it.
 */
export async function fireVetoableHook(fire) {
    try {
        await fire();
    }
    catch (error) {
        if (typeof error === 'object' &&
            error !== null &&
            !error.code) {
            try {
                ;
                error.code = 'wasp-auth/policy-veto';
            }
            catch {
                // A frozen error object stays untagged; it still propagates.
            }
        }
        throw error;
    }
}
export const onBeforeSignupHook = (params) => onBeforeSignupHook_ext({ prisma, ...params });
export const onAfterSignupHook = async (_params) => { };
export const onBeforeLoginHook = async (_params) => { };
export const onAfterLoginHook = (params) => onAfterLoginHook_ext({ prisma, ...params });
//# sourceMappingURL=hookDispatch.js.map