import type { Router, Request } from 'express'
import type { User } from '../../entities'
import type { Expand } from '../../universal/types'

export type ProviderConfig = {
    // Unique provider identifier, used as part of URL paths
    id: string;
    displayName: string;
    // Each provider config can have an init method which is ran on setup time
    // e.g. for oAuth providers this is the time when the Passport strategy is registered.
    init?(provider: ProviderConfig): Promise<InitData>;
    // Every provider must have a setupRouter method which returns the Express router.
    // In this function we are flexibile to do what ever is necessary to make the provider work.
    createRouter(provider: ProviderConfig, initData: InitData): Router;
};

export type InitData = {
    [key: string]: any;
}

export type RequestWithWasp = Request & { wasp?: { [key: string]: any } }

export type AdditionalSignupFieldsConfig<
    DisallowedFields extends keyof User,
> = Expand<
  Omit<
    Partial<{
      [key in keyof User]: {
        get: (value: string) => unknown;
        // Expected to throw an error if validation fails
        validate: (value: string) => void;
      };
    }>,
    DisallowedFields
  >
>;