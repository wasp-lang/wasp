import { env } from "wasp/server";

const headers = {
  "Content-Type": "application/json",
  Authorization: `Bearer ${env.PLAUSIBLE_API_KEY}`,
};

type PageViewsResult = {
  results: {
    [key: string]: {
      value: number;
    };
  };
};

type PageViewSourcesResult = {
  results: [
    {
      source: string;
      visitors: number;
    },
  ];
};

export async function getDailyPageViews() {
  const totalViews = await getTotalPageViews();
  const prevDayViewsChangePercent = await getPrevDayViewsChangePercent();

  return {
    totalViews,
    prevDayViewsChangePercent,
  };
}

async function getTotalPageViews() {
  const response = await fetch(
    `${env.PLAUSIBLE_BASE_URL}/v1/stats/aggregate?site_id=${env.PLAUSIBLE_SITE_ID}&metrics=pageviews`,
    {
      method: "GET",
      headers,
    },
  );
  if (!response.ok) {
    throw new Error(`HTTP error! Status: ${response.status}`);
  }
  const json = (await response.json()) as PageViewsResult;

  return json.results.pageviews.value;
}

async function getPrevDayViewsChangePercent() {
  // Calculate today, yesterday, and the day before yesterday's dates
  const today = new Date();
  const yesterday = new Date(today.setDate(today.getDate() - 1))
    .toISOString()
    .split("T")[0];
  const dayBeforeYesterday = new Date(
    new Date().setDate(new Date().getDate() - 2),
  )
    .toISOString()
    .split("T")[0];

  // Fetch page views for yesterday and the day before yesterday
  const pageViewsYesterday = await getPageviewsForDate(yesterday);
  const pageViewsDayBeforeYesterday =
    await getPageviewsForDate(dayBeforeYesterday);

  console.table({
    pageViewsYesterday,
    pageViewsDayBeforeYesterday,
    typeY: typeof pageViewsYesterday,
    typeDBY: typeof pageViewsDayBeforeYesterday,
  });

  if (pageViewsYesterday === 0 || pageViewsDayBeforeYesterday === 0) {
    return "0";
  }
  const change =
    ((pageViewsYesterday - pageViewsDayBeforeYesterday) /
      pageViewsDayBeforeYesterday) *
    100;
  return change.toFixed(0);
}

async function getPageviewsForDate(date: string) {
  const url = `${env.PLAUSIBLE_BASE_URL}/v1/stats/aggregate?site_id=${env.PLAUSIBLE_SITE_ID}&period=day&date=${date}&metrics=pageviews`;
  const response = await fetch(url, {
    method: "GET",
    headers,
  });
  if (!response.ok) {
    throw new Error(`HTTP error! Status: ${response.status}`);
  }
  const data = (await response.json()) as PageViewsResult;
  return data.results.pageviews.value;
}

export async function getSources() {
  const url = `${env.PLAUSIBLE_BASE_URL}/v1/stats/breakdown?site_id=${env.PLAUSIBLE_SITE_ID}&property=visit:source&metrics=visitors`;
  const response = await fetch(url, {
    method: "GET",
    headers,
  });
  if (!response.ok) {
    throw new Error(`HTTP error! Status: ${response.status}`);
  }
  const data = (await response.json()) as PageViewSourcesResult;
  return data.results;
}
