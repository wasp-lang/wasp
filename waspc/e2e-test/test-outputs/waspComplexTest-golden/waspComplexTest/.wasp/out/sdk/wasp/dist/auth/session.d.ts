import { Request as ExpressRequest } from "express";
import { type AuthUser } from 'wasp/auth';
import type { Session } from "lucia";
export declare function createSession(authId: string): Promise<Session>;
export declare function getSessionAndUserFromBearerToken(req: ExpressRequest): Promise<{
    user: AuthUser | null;
    session: Session | null;
}>;
export declare function getSessionAndUserFromSessionId(sessionId: string): Promise<{
    user: AuthUser | null;
    session: Session | null;
}>;
export declare function invalidateSession(sessionId: string): Promise<void>;
