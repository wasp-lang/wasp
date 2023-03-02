import passport from "passport";

import waspServerConfig from '../../../config.js';
import { contextWithUserEntity } from "../../utils";

import { InitData, ProviderConfig, OAuthConfig, RequestWithWasp } from "../types.js";

export function makeOAuthInit({ userDefinedConfigFn, getUserFieldsFn, npmPackage, oAuthConfig }: OAuthImports) {
    return async function init(provider: ProviderConfig): Promise<InitData> {
        const userDefinedConfig = userDefinedConfigFn
            ? await userDefinedConfigFn()
            : {};
        const ProviderStrategy = await import(npmPackage);

        const passportStrategyName = `wasp${provider.slug}LoginStrategy`;
        const requiredConfig = {
            clientID: oAuthConfig.clientID,
            clientSecret: oAuthConfig.clientSecret,
            scope: oAuthConfig.scope,
            callbackURL: `${waspServerConfig.frontendUrl}/auth/login/${provider.slug}`,
            passReqToCallback: true
        };

        const config = {
            ...requiredConfig,
            ...userDefinedConfig,
        };
        ensureValidConfig(provider, config);

        const passportStrategy = new ProviderStrategy.default(
            config,
            addProviderProfileToRequest
        );
        passport.use(passportStrategyName, passportStrategy);

        return {
            passportStrategyName,
            getUserFieldsFn,
        };
    }
}

// This function is invoked after we successfully exchange the one-time-use OAuth code for a real provider API token.
// This token was used to get the provider profile information supplied as a parameter.
// We add the provider profile to the request for downstream use.
async function addProviderProfileToRequest(
    req: RequestWithWasp,
    _accessToken: string,
    _refreshToken: string,
    providerProfile: { [key: string]: any },
    done: any,
) {
    req.wasp = { ...req.wasp, providerProfile };
    done(null, {});
}

function ensureValidConfig(provider: ProviderConfig, config: OAuthConfig): void {
    if (!config.clientID) {
        throw new Error(`The ${provider.name} auth provider requires clientID provided via env varibales.`)
    }

    if (!config.clientSecret) {
        throw new Error(`The ${provider.name} auth provider requires clientSecret provided via env varibales.`)
    }

    if (!config.scope || !Array.isArray(config.scope)) {
        throw new Error(`The ${provider.name} auth provider requires scope.`)
    }
}

type OAuthImports = {
    npmPackage: string;
    userDefinedConfigFn?: () => Promise<{ [key: string]: any }>;
    getUserFieldsFn: (
        context: typeof contextWithUserEntity,
        args: { profile: { [key: string]: any } }
    ) => Promise<{ [key: string]: any }>;
    oAuthConfig: OAuthConfig;
};
