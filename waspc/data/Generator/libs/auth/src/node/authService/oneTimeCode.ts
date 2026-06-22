import type { AuthId } from "./types";

export type OneTimeCodeStore = {
  createToken(authId: AuthId): Promise<string>;
  verifyToken(code: string): Promise<{ authId: AuthId }>;
  isUsed(code: string): boolean;
  markUsed(code: string): void;
};
