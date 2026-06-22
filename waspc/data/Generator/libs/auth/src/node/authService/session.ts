import type { AuthId } from "./types";

export type Session = {
  id: string;
};

export type SessionService = {
  createSession(authId: AuthId): Promise<Session>;
};
