import { Project } from "@wasp/entities";

export function generateLast24HoursData(projects: Pick<Project, "createdAt">[]) {
  const buckets: {
    date: Date;
    displayValue: number;
    count: number;
  }[] = [];

  const now = new Date();
  const last24Hours = new Date(now.getTime() - 24 * 60 * 60 * 1000);
  for (let i = 0; i < 24; i++) {
    const bucketStart = new Date(last24Hours.getTime() + i * 60 * 60 * 1000);
    const bucket = {
      date: bucketStart,
      displayValue: bucketStart.getHours() + 1,
      count: 0,
    };
    buckets.push(bucket);
  }
  projects.forEach((project) => {
    const createdAt = new Date(project.createdAt);
    // Difference in hours between now and when the project was created
    const bucketIndex = Math.floor((now.getTime() - createdAt.getTime()) / (60 * 60 * 1000));
    const reverseBucketIndex = buckets.length - bucketIndex - 1;
    // Count only projects that were created in the last 24 hours
    if (bucketIndex >= 0 && bucketIndex < 24) {
      buckets[reverseBucketIndex].count++;
    }
  });
  return buckets;
}

export function generateLast30DaysData(projects: Pick<Project, "createdAt">[]) {
  const buckets: {
    date: Date;
    displayValue: number;
    count: number;
  }[] = [];

  const now = new Date();
  const last30Days = new Date(now.getTime() - 30 * 24 * 60 * 60 * 1000);
  for (let i = 0; i < 30; i++) {
    const bucketStart = new Date(last30Days.getTime() + i * 24 * 60 * 60 * 1000);
    const bucket = {
      date: bucketStart,
      displayValue: bucketStart.getDate(),
      count: 0,
    };
    buckets.push(bucket);
  }
  projects.forEach((project) => {
    const createdAt = new Date(project.createdAt);
    // Difference in days between now and when the project was created
    const bucketIndex = Math.floor((now.getTime() - createdAt.getTime()) / (24 * 60 * 60 * 1000));
    const reverseBucketIndex = buckets.length - bucketIndex - 1;
    // Count only projects that were created in the last 30 days
    if (bucketIndex >= 0 && bucketIndex < 30) {
      buckets[reverseBucketIndex].count++;
    }
  });
  return buckets;
}
