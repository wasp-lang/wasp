// All of our docs are written in US English, so our formatters should also use it.
export const LOCALE = "en-US";

export const listFormatter = new Intl.ListFormat(LOCALE, {
  type: "conjunction",
});

export const dateFormatter = new Intl.DateTimeFormat(LOCALE, {
  year: "numeric",
  month: "short",
  day: "numeric",
  timeZone: "UTC", // Ensure consistent date formatting regardless of user's timezone
});
