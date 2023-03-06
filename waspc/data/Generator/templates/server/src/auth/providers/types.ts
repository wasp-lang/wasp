import type { Router, Request } from "express";

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

export type OAuthConfig = {
    clientID?: string;
    clientSecret?: string;
    scope?: string[];
}

export type RequestWithWasp = Request & { wasp?: { [key: string]: any } }
