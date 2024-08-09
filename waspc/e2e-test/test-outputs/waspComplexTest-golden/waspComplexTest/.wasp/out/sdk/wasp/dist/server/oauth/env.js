// PRIVATE API (SDK)
export function ensureEnvVarsForProvider(envVarNames, providerName) {
    const result = {};
    for (const envVarName of envVarNames) {
        const value = process.env[envVarName];
        if (!value) {
            throw new Error(`${envVarName} env variable is required when using the ${providerName} auth provider.`);
        }
        result[envVarName] = value;
    }
    return result;
}
//# sourceMappingURL=env.js.map