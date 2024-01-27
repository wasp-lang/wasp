import passport from "passport";

import { config as waspServerConfig } from 'wasp/server';

import type { InitData, ProviderConfig, RequestWithWasp, UserSignupFields } from "wasp/auth/providers/types";
import type { OAuthConfig, UserDefinedConfigFn  } from "./types.js";

export function makeOAuthInit({ userDefinedConfigFn, userSignupFields, npmPackage, oAuthConfig }: OAuthImports) {
    return async function init(provider: ProviderConfig): Promise<InitData> {
        const userDefinedConfig = userDefinedConfigFn
            ? userDefinedConfigFn()
            : {};
        const ProviderStrategy = await import(npmPackage);

        const passportStrategyName = `wasp${provider.id}LoginStrategy`;
        const requiredConfig = {
            clientID: oAuthConfig.clientID,
            clientSecret: oAuthConfig.clientSecret,
            scope: oAuthConfig.scope,
            callbackURL: `${waspServerConfig.frontendUrl}/auth/login/${provider.id}`,
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
            userSignupFields,
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
        throw new Error(`The ${provider.displayName} auth provider requires clientID provided via env variables.`)
    }

    if (!config.clientSecret) {
        throw new Error(`The ${provider.displayName} auth provider requires clientSecret provided via env variables.`)
    }

    if (!config.scope || !Array.isArray(config.scope)) {
        throw new Error(`The ${provider.displayName} auth provider requires scope.`)
    }
}

export type OAuthImports = {
    npmPackage: string;
    userDefinedConfigFn?: UserDefinedConfigFn;
    oAuthConfig: OAuthConfig;
    userSignupFields?: UserSignupFields;
};
