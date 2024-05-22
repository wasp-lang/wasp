import type { Request as ExpressRequest } from 'express';
import type { ProviderId, createUser } from '../../auth/utils.js';
import { prisma } from '../index.js';
type CommonInput = {
    hookName: string;
    prisma: typeof prisma;
    req: ExpressRequest;
};
export type OnBeforeSignupHookFn = (params: OnBeforeSignupHookFnInput) => Promise<void>;
type OnBeforeSignupHookFnInput = {
    providerId: ProviderId;
} & CommonInput;
export type OnAfterSignupHookFn = (params: OnAfterSignupHookFnInput) => Promise<void>;
type OnAfterSignupHookFnInput = {
    providerId: ProviderId;
    user: Awaited<ReturnType<typeof createUser>>;
} & CommonInput;
export type OnBeforeOAuthRedirectHookFn = (params: OnBeforeOAuthRedirectHookFnInput) => Promise<{
    url: URL;
}>;
type OnBeforeOAuthRedirectHookFnInput = {
    url: URL;
} & CommonInput;
export type OnAfterOAuthTokenReceivedHookFn = (params: OnAfterOAuthTokenReceivedHookFnInput) => Promise<void>;
type OnAfterOAuthTokenReceivedHookFnInput = {
    accessToken: string;
} & CommonInput;
export {};
