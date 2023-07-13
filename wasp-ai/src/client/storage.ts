const REFERRER_KEY = "ref";
const UNKNOWN_REFERRER = "unknown";

export function saveReferrerToLocalStorage(urlParams: URLSearchParams) {
  const newRefValue = urlParams.get(REFERRER_KEY) ?? UNKNOWN_REFERRER;
  const oldRefValue = localStorage.getItem(REFERRER_KEY);
  if (oldRefValue === null) {
    localStorage.setItem(REFERRER_KEY, newRefValue);
  }
}

export function readReferrerFromLocalStorage() {
  return localStorage.getItem(REFERRER_KEY) ?? UNKNOWN_REFERRER;
}
