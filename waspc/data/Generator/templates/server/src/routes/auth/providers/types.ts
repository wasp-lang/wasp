import type { Router } from "express";

export const ProviderType = {
    google: "google",
    local: "local",
    github: "github"
} as const;

export type ProviderType = (typeof ProviderType)[keyof typeof ProviderType];

export type ProviderConfig = {
    name: ProviderType;
    slug: string; // it might differ from name, e.g. google -> google-oauth2
    init?(): { [key: string]: any };
    setupRouter(): Router;
};
