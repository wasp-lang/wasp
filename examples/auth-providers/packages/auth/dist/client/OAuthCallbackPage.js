import { jsx as _jsx } from "react/jsx-runtime";
import { useState } from "react";
import { Navigate, useLocation } from "react-router";
import { exchangeOAuthCodeForSession } from "./actions.js";
import { Message, MessageError } from "./forms/internal/Message.js";
import { useEffectOnce } from "./hooks.js";
import { getClientOptions } from "./runtime.js";
/** The client route the OAuth handback lands on: redeems the one-time code, then redirects. */
export function OAuthCallbackPage() {
    const [error, setError] = useState(null);
    const [isDone, setIsDone] = useState(false);
    const location = useLocation();
    useEffectOnce(() => {
        (async () => {
            const query = new URLSearchParams(location.search);
            const errorFromRedirect = query.get("error");
            if (errorFromRedirect !== null) {
                setError(errorFromRedirect);
                return;
            }
            try {
                await exchangeOAuthCodeForSession(location.hash.slice(1));
                setIsDone(true);
            }
            catch (e) {
                console.error(e);
                setError("Unable to login with the OAuth provider.");
            }
        })();
    });
    if (isDone) {
        return (_jsx(Navigate, { to: getClientOptions().onAuthSucceededRedirectTo, replace: true }));
    }
    if (error) {
        return _jsx(MessageError, { children: error });
    }
    return _jsx(Message, { children: "Please wait a moment while we log you in." });
}
