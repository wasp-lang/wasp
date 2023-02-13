export function isValidAbsoluteURL(rawUrl) {
    try {
        new URL(rawUrl);
    } catch (e) {
        return false;
    }

    /*
        URL constrctor will parse some invalid absolute URLs as valid URLs
        so we need to do some extra checks to make sure the URL is valid.

        Example: "localhost:3000" will be parsed as URL with protocol of
        "localhost:" and host of "3000"
    */
    const urlParts = rawUrl.split("//");
    const isProtocolSpecified = urlParts.length > 1;
    if (!isProtocolSpecified) {
        return false;
    }

    const protocol = urlParts[0];

    return protocol === "http:" || protocol === "https:";
}
