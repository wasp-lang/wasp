import { ProviderConfig, ProviderType } from '../types';

export const config: ProviderConfig = {
    name: ProviderType.local,
    slug: "local",
    setupRouter: () => {},
}
