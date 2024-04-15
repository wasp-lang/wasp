import type { AuthUser } from '../server/auth/user.js';
import { UseQueryResult } from '@tanstack/react-query';
export declare const getMe: () => Promise<AuthUser | null>;
export default function useAuth(queryFnArgs?: unknown, config?: any): UseQueryResult<AuthUser>;
