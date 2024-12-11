import type { Request as ExpressRequest } from 'express';
import { type ProviderId, type CreateUserResult, type FindAuthWithUserResult } from '../../auth/utils.js';
import { prisma } from '../index.js';
import { Expand } from '../../universal/types.js';
export type OnBeforeSignupHook = (params: Expand<OnBeforeSignupHookParams>) => void | Promise<void>;
export type OnAfterSignupHook = (params: Expand<OnAfterSignupHookParams>) => void | Promise<void>;
/**
 * @returns Object with a URL that the OAuth flow should redirect to.
 */
export type OnBeforeOAuthRedirectHook = (params: Expand<OnBeforeOAuthRedirectHookParams>) => {
    url: URL;
} | Promise<{
    url: URL;
}>;
export type OnBeforeLoginHook = (params: Expand<OnBeforeLoginHookParams>) => void | Promise<void>;
export type OnAfterLoginHook = (params: Expand<OnAfterLoginHookParams>) => void | Promise<void>;
export type InternalAuthHookParams = {
    /**
     * Prisma instance that can be used to interact with the database.
    */
    prisma: typeof prisma;
};
type OnBeforeSignupHookParams = {
    /**
     * Provider ID object that contains the provider name and the provide user ID.
    */
    providerId: ProviderId;
    /**
     * Request object that can be used to access the incoming request.
    */
    req: ExpressRequest;
} & InternalAuthHookParams;
type OnAfterSignupHookParams = {
    /**
     * Provider ID object that contains the provider name and the provide user ID.
    */
    providerId: ProviderId;
    /**
     * User object that was created during the signup process.
    */
    user: CreateUserResult;
    /**
     * OAuth flow data that was generated during the OAuth flow. This is only
     * available if the user signed up using OAuth.
    */
    oauth?: OAuthData;
    /**
     * Request object that can be used to access the incoming request.
    */
    req: ExpressRequest;
} & InternalAuthHookParams;
type OnBeforeOAuthRedirectHookParams = {
    /**
     * URL that the OAuth flow should redirect to.
    */
    url: URL;
    /**
     * Unique request ID that was generated during the OAuth flow.
    */
    oauth: Pick<OAuthData, 'uniqueRequestId'>;
    /**
     * Request object that can be used to access the incoming request.
    */
    req: ExpressRequest;
} & InternalAuthHookParams;
type OnBeforeLoginHookParams = {
    /**
     * Provider ID object that contains the provider name and the provide user ID.
    */
    providerId: ProviderId;
    /**
     * User that is trying to log in.
    */
    user: FindAuthWithUserResult['user'];
    /**
     * Request object that can be used to access the incoming request.
    */
    req: ExpressRequest;
} & InternalAuthHookParams;
type OnAfterLoginHookParams = {
    /**
     * Provider ID object that contains the provider name and the provide user ID.
    */
    providerId: ProviderId;
    /**
     * User that is logged in.
    */
    user: FindAuthWithUserResult['user'];
    /**
     * OAuth flow data that was generated during the OAuth flow. This is only
     * available if the user logged in using OAuth.
    */
    oauth?: OAuthData;
    /**
     * Request object that can be used to access the incoming request.
    */
    req: ExpressRequest;
} & InternalAuthHookParams;
export type OAuthData = {
    /**
     * Unique request ID that was generated during the OAuth flow.
    */
    uniqueRequestId: string;
} & ({
    providerName: 'google';
    tokens: import('arctic').GoogleTokens;
} | never);
export {};
