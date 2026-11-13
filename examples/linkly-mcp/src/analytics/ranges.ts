// Shared analytics vocabulary. Lives in its own file so deep-link builders
// and schemas can use it without importing the analytics service.
export const RANGES = ["24h", "7d", "30d", "90d"] as const;
export type Range = (typeof RANGES)[number];
export const GROUP_BYS = ["day", "referrer"] as const;
export type GroupBy = (typeof GROUP_BYS)[number];

export const RANGE_LABELS: Record<Range, string> = {
  "24h": "Last 24 hours",
  "7d": "Last 7 days",
  "30d": "Last 30 days",
  "90d": "Last 90 days",
};
