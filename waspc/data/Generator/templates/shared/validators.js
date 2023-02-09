export function isValidAbsoluteURL(rawUrl) {
    try {
        new URL(rawUrl);
    } catch (e) {
        // Invalid URL
        return false;
    }

    const urlParts = rawUrl.split("//");

    // No protocol specified
    if (urlParts.length < 2) {
        return false;
    }

    const protocol = urlParts[0];
    // Invalid protocol specified
    if (protocol !== "http:" && protocol !== "https:") {
        return false;
    }

    return true;
}
