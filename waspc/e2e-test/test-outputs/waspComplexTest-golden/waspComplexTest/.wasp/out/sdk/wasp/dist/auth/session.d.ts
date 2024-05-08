import { Request as ExpressRequest } from "express";
import { type AuthUserData } from '../server/auth/user.js';
import type { Session } from "lucia";
export declare function createSession(authId: string): Promise<Session>;
type UserAndSession = {
    user: AuthUserData;
    session: Session;
};
export declare function getSessionAndUserFromBearerToken(req: ExpressRequest): Promise<UserAndSession | null>;
export declare function getSessionAndUserFromSessionId(sessionId: string): Promise<UserAndSession | null>;
export declare function invalidateSession(sessionId: string): Promise<void>;
export {};
