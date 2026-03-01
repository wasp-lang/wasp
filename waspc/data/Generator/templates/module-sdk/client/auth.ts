import type { AuthUser } from '../server/module/index';

export type { AuthUser };

export declare function useAuth(): {
  data: AuthUser | undefined;
  isLoading: boolean;
  error: Error | null;
};

export declare function logout(): Promise<void>;
