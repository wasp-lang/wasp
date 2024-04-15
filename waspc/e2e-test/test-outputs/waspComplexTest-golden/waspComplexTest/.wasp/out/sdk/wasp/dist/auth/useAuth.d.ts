import type { AuthUser } from './user.js';
import { UseQueryResult } from '@tanstack/react-query';
export declare const getMe: () => Promise<AuthUser | null>;
export default function useAuth(queryFnArgs?: unknown, config?: any): UseQueryResult<AuthUser>;
