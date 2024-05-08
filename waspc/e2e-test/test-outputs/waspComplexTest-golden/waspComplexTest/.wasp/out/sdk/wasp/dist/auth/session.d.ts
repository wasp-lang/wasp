import { Request as ExpressRequest } from "express";
import { type AuthUserData } from 'wasp/auth';
import type { Session } from "lucia";
export declare function createSession(authId: string): Promise<Session>;
export declare function getSessionAndUserFromBearerToken(req: ExpressRequest): Promise<{
    user: AuthUserData | null;
    session: Session | null;
}>;
export declare function getSessionAndUserFromSessionId(sessionId: string): Promise<{
    user: AuthUserData | null;
    session: Session | null;
}>;
export declare function invalidateSession(sessionId: string): Promise<void>;
