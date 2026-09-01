import { jsx as _jsx, jsxs as _jsxs } from "react/jsx-runtime";
import { useEffect, useState } from "react";
/**
 * The client half of the externalized wasp auth: plain React exports plus a
 * handful of fetch helpers, all built on the two things Wasp hands a client
 * adapter -- `apiUrl` and the provider-bound `setSession` sink. No generated
 * imports anywhere; the package versions independently of any app.
 */
let runtime = null;
let options = null;
export const createClientAdapter = (waspRuntime, adapterOptions) => {
    runtime = waspRuntime;
    options = adapterOptions;
    // No Wrapper, no ambient credential: sessions are adopted explicitly via
    // the setSession sink at the moment a login route returns one.
    return {};
};
function getRuntime() {
    if (runtime === null) {
        throw new Error("@wasp.sh/auth client used before Wasp instantiated its adapter. Is the provider declared in main.wasp.ts?");
    }
    return runtime;
}
async function post(path, body) {
    const response = await fetch(`${getRuntime().apiUrl}/wasp-auth${path}`, {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify(body),
    });
    const data = (await response.json().catch(() => ({})));
    return { ok: response.ok, status: response.status, data };
}
async function adoptSessionFrom(data) {
    if (typeof data.sessionId !== "string") {
        throw new Error("Login did not return a session.");
    }
    await getRuntime().setSession(data.sessionId);
}
// PUBLIC API -- one helper per in-tree client action.
export async function signup(username, password) {
    const { ok, data } = await post("/username/signup", { username, password });
    if (!ok)
        throw new Error(String(data.message ?? "Signup failed"));
}
export async function login(username, password) {
    const { ok, data } = await post("/username/login", { username, password });
    if (!ok)
        throw new Error(String(data.message ?? "Login failed"));
    await adoptSessionFrom(data);
}
export async function emailSignup(email, password) {
    const { ok, data } = await post("/email/signup", { email, password });
    if (!ok)
        throw new Error(String(data.message ?? "Signup failed"));
}
export async function emailLogin(email, password) {
    const { ok, data } = await post("/email/login", { email, password });
    if (!ok)
        throw new Error(String(data.message ?? "Login failed"));
    await adoptSessionFrom(data);
}
export async function verifyEmail(token) {
    const { ok, data } = await post("/email/verify", { token });
    if (!ok)
        throw new Error(String(data.message ?? "Verification failed"));
}
export async function requestPasswordReset(email) {
    const { ok, data } = await post("/email/request-password-reset", { email });
    if (!ok)
        throw new Error(String(data.message ?? "Request failed"));
}
export async function resetPassword(token, password) {
    const { ok, data } = await post("/email/reset-password", { token, password });
    if (!ok)
        throw new Error(String(data.message ?? "Reset failed"));
}
/** Navigate here to start the Google login dance. */
export function googleLoginUrl() {
    return `${getRuntime().apiUrl}/wasp-auth/google/login`;
}
export async function exchangeOAuthCode(code) {
    const { ok, data } = await post("/exchange-code", { code });
    if (!ok)
        throw new Error(String(data.message ?? "Code exchange failed"));
    await adoptSessionFrom(data);
}
// PUBLIC API -- minimal pages, the in-tree Auth UI's job in plainest form.
export function AuthForm({ onSuccess }) {
    const methods = options?.methods ?? {};
    const [mode, setMode] = useState("login");
    const [identifier, setIdentifier] = useState("");
    const [password, setPassword] = useState("");
    const [message, setMessage] = useState(null);
    const useEmail = methods.email !== undefined;
    async function submit(e) {
        e.preventDefault();
        setMessage(null);
        try {
            if (useEmail) {
                if (mode === "signup") {
                    await emailSignup(identifier, password);
                    setMessage("Check your email for a verification link.");
                    return;
                }
                await emailLogin(identifier, password);
            }
            else {
                if (mode === "signup") {
                    await signup(identifier, password);
                }
                await login(identifier, password);
            }
            onSuccess?.();
        }
        catch (error) {
            setMessage(error instanceof Error ? error.message : "Something went wrong");
        }
    }
    return (_jsxs("div", { style: { maxWidth: 380, margin: "3rem auto", fontFamily: "system-ui" }, children: [_jsx("h1", { children: mode === "signup" ? "Sign up" : "Log in" }), _jsx("p", { style: { color: "#666" }, children: "Wasp auth, running from a package" }), _jsxs("form", { onSubmit: submit, children: [_jsx("input", { value: identifier, onChange: (e) => setIdentifier(e.target.value), placeholder: useEmail ? "email" : "username" }), _jsx("input", { type: "password", value: password, onChange: (e) => setPassword(e.target.value), placeholder: "password" }), _jsx("button", { type: "submit", children: mode === "signup" ? "Sign up" : "Log in" })] }), methods.google !== undefined ? (_jsx("p", { children: _jsx("a", { href: googleLoginUrl(), children: "Continue with Google" }) })) : null, message ? _jsx("p", { style: { color: "crimson" }, children: message }) : null, _jsx("button", { onClick: () => setMode(mode === "login" ? "signup" : "login"), children: mode === "login" ? "I need an account" : "I already have an account" })] }));
}
export function OAuthCallbackPage({ redirectTo = "/" }) {
    const [error, setError] = useState(null);
    useEffect(() => {
        const code = window.location.hash.slice(1);
        if (!code) {
            setError("Missing login code.");
            return;
        }
        exchangeOAuthCode(code)
            .then(() => window.location.replace(redirectTo))
            .catch((e) => setError(e instanceof Error ? e.message : "Login failed"));
    }, [redirectTo]);
    return _jsx("p", { style: { fontFamily: "system-ui" }, children: error ?? "Signing you in..." });
}
export function VerifyEmailPage({ loginPath = "/login" }) {
    const [message, setMessage] = useState("Verifying...");
    useEffect(() => {
        const token = new URLSearchParams(window.location.search).get("token");
        if (!token) {
            setMessage("Missing verification token.");
            return;
        }
        verifyEmail(token)
            .then(() => setMessage("Email verified. You can log in now."))
            .catch((e) => setMessage(e instanceof Error ? e.message : "Verification failed"));
    }, []);
    return (_jsxs("p", { style: { fontFamily: "system-ui" }, children: [message, " ", _jsx("a", { href: loginPath, children: "Log in" })] }));
}
export function PasswordResetPage({ loginPath = "/login" }) {
    const [password, setPassword] = useState("");
    const [message, setMessage] = useState(null);
    async function submit(e) {
        e.preventDefault();
        const token = new URLSearchParams(window.location.search).get("token");
        if (!token) {
            setMessage("Missing reset token.");
            return;
        }
        try {
            await resetPassword(token, password);
            setMessage("Password changed. You can log in now.");
        }
        catch (error) {
            setMessage(error instanceof Error ? error.message : "Reset failed");
        }
    }
    return (_jsxs("div", { style: { maxWidth: 380, margin: "3rem auto", fontFamily: "system-ui" }, children: [_jsx("h1", { children: "Reset password" }), _jsxs("form", { onSubmit: submit, children: [_jsx("input", { type: "password", value: password, onChange: (e) => setPassword(e.target.value), placeholder: "new password" }), _jsx("button", { type: "submit", children: "Reset" })] }), message ? (_jsxs("p", { children: [message, " ", _jsx("a", { href: loginPath, children: "Log in" })] })) : null] }));
}
