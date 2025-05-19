import type { Router, Request } from 'express';
import type { Prisma } from '@prisma/client';
import type { Expand, Exact } from 'wasp/universal/types';
import type { ProviderName } from '../utils';
export declare function defineUserSignupFields<T extends UserSignupFields>(fields: Exact<UserSignupFields, T>): T;
type UserEntityCreateInput = Prisma.UserCreateInput;
export type ProviderConfig = {
    id: ProviderName;
    displayName: string;
    createRouter(provider: ProviderConfig): Router;
};
export type RequestWithWasp = Request & {
    wasp?: {
        [key: string]: any;
    };
};
export type PossibleUserFields = Expand<Partial<UserEntityCreateInput>>;
export type UserSignupFields = {
    [key in keyof PossibleUserFields]: FieldGetter<PossibleUserFields[key]>;
};
type FieldGetter<T extends PossibleUserFieldValues> = (data: {
    [key: string]: unknown;
}) => Promise<T | undefined> | T | undefined;
type PossibleUserFieldValues = PossibleUserFields[keyof PossibleUserFields];
export {};
//# sourceMappingURL=types.d.ts.map