import { Request as ExpressRequest } from "express";
import { type AuthUserData } from '../server/auth/user.js';
import type { Session } from "lucia";
export declare function createSession(authId: string): Promise<Session>;
type SessionAndUser = {
    session: Session;
    user: AuthUserData;
};
export declare function getSessionAndUserFromBearerToken(req: ExpressRequest): Promise<SessionAndUser | null>;
export declare function getSessionAndUserFromSessionId(sessionId: string): Promise<SessionAndUser | null>;
export declare function invalidateSession(sessionId: string): Promise<void>;
export {};
//# sourceMappingURL=session.d.ts.map