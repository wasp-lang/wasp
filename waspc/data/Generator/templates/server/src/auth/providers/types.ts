import type { Router } from "express";

export type ProviderConfig = {
    slug: string;
    init?(provider: ProviderConfig): Promise<InitData>;
    setupRouter(provider: ProviderConfig, initData: InitData): Router;
};

export type InitData = {
    [key: string]: any;
}
