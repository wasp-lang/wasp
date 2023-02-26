import type { Router } from "express";

export type ProviderConfig = {
    // The name of the provider, e.g. "Google".
    name: string;
    // The slug of the provider, e.g. "google".
    slug: string;
    init?(provider: ProviderConfig): Promise<InitData>;
    setupRouter(provider: ProviderConfig, initData: InitData): Router;
};

export type InitData = {
    [key: string]: any;
}

export type OAuthConfig = {
    clientID?: string;
    clientSecret?: string;
    scope?: string[];
}
