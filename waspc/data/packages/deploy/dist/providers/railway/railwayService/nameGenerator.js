export function createRailwayClientServiceName(projectName) {
    return createRailwayServiceName(projectName, ServiceWithSuffixedName.Client);
}
export function createRailwayServerServiceName(projectName) {
    return createRailwayServiceName(projectName, ServiceWithSuffixedName.Server);
}
export function createRailwayDbServiceName() {
    // Kept as "Postgres" for compatibility with databases created by earlier
    // Wasp versions.
    return "Postgres";
}
function createRailwayServiceName(projectName, service) {
    const serviceNameSuffix = serviceNameSuffixes[service];
    return `${projectName}${serviceNameSuffix}`;
}
var ServiceWithSuffixedName;
(function (ServiceWithSuffixedName) {
    ServiceWithSuffixedName["Client"] = "Client";
    ServiceWithSuffixedName["Server"] = "Server";
})(ServiceWithSuffixedName || (ServiceWithSuffixedName = {}));
export const serviceNameSuffixes = {
    [ServiceWithSuffixedName.Client]: "-client",
    [ServiceWithSuffixedName.Server]: "-server",
};
//# sourceMappingURL=nameGenerator.js.map