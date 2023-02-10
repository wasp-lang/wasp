import type { Router } from "express";

export const ProviderType = {
    google: "google",
    local: "local",
    github: "github"
} as const;

export type ProviderType = (typeof ProviderType)[keyof typeof ProviderType];

export type ProviderConfig = {
    name: ProviderType;
    slug: string;
    init?(provider: ProviderConfig): Promise<InitData>;
    setupRouter(provider: ProviderConfig, initData: InitData): Router;
};

export type InitData = {
    [key: string]: any;
}
