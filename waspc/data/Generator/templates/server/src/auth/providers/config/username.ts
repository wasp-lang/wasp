{{={= =}=}}

import { Router } from "express";

import login from "../username/login.js";
import { getSignupRoute } from "../username/signup.js";
import { ProviderConfig } from "wasp/auth/providers/types";

{=# userSignupFields.isDefined =}
{=& userSignupFields.importStatement =}
const _waspUserSignupFields = {= userSignupFields.importIdentifier =}
{=/ userSignupFields.isDefined =}
{=^ userSignupFields.isDefined =}
const _waspUserSignupFields = undefined
{=/ userSignupFields.isDefined =}

const config: ProviderConfig = {
    id: "{= providerId =}",
    displayName: "{= displayName =}",
    createRouter() {
        const router = Router();

        router.post('/login', login);
        const signupRoute = getSignupRoute({
            userSignupFields: _waspUserSignupFields,
        });
        router.post('/signup', signupRoute);

        return router;
    },
}

export default config;
