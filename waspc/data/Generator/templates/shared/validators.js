export function isValidAbsoluteURL(rawUrl) {
    try {
        const url = new URL(rawUrl);
        /*
            URL constructor will parse some invalid absolute URLs as valid URLs
            so we need to do some extra checks.

            Example: "localhost:3000" will be parsed as URL with protocol of
            "localhost:" and host of "3000"
        */
        return url.protocol === "http:" || url.protocol === "https:";
    } catch (e) {
        return false;
    }
}
