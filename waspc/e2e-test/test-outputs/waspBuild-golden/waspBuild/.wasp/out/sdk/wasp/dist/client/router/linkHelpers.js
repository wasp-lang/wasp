// PRIVATE API
export function interpolatePath(path, params, search, hash) {
    const interpolatedPath = params ? interpolatePathParams(path, params) : path;
    const interpolatedSearch = search
        ? `?${new URLSearchParams(search).toString()}`
        : '';
    const interpolatedHash = hash ? `#${hash}` : '';
    return interpolatedPath + interpolatedSearch + interpolatedHash;
}
function interpolatePathParams(path, params) {
    function mapPathPart(part) {
        if (part === '*') {
            return params['*'];
        }
        if (part.startsWith(":")) {
            const paramName = extractParamNameFromPathPart(part);
            return params[paramName];
        }
        return part;
    }
    const interpolatedPath = path
        .split("/")
        .map(mapPathPart)
        .filter(isValidPathPart)
        .join("/");
    return path.startsWith("/") ? `/${interpolatedPath}` : interpolatedPath;
}
function isValidPathPart(part) {
    return !!part;
}
function extractParamNameFromPathPart(paramString) {
    if (paramString.endsWith("?")) {
        return paramString.slice(1, -1);
    }
    return paramString.slice(1);
}
//# sourceMappingURL=linkHelpers.js.map