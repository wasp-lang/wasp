import type { AuthUser } from './types';
export declare const getMe: () => Promise<AuthUser | null>;
export default function useAuth(queryFnArgs?: unknown, config?: any): import("@tanstack/react-query").UseQueryResult<AuthUser, Error>;
