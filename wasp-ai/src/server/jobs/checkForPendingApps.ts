import { generateAppJob } from "@wasp/jobs/generateAppJob.js";
import { log } from "./utils.js";

const maxProjectsInProgress = process.env.MAX_PROJECTS_IN_PROGRESS
  ? parseInt(process.env.MAX_PROJECTS_IN_PROGRESS, 10)
  : 7;

export async function checkForPendingApps(
  _args: void,
  context: {
    entities: {
      Project: any;
    };
  }
) {
  log("Checking for pending apps");
  const { Project } = context.entities;

  const pendingProjects = await Project.findMany({
    where: { status: "pending" },
    orderBy: { createdAt: "asc" },
  });
  const inProgressProjects = await Project.findMany({
    where: { status: "in-progress" },
  });

  // Generate X new apps until there are `maxProjectsInProgress` in progress.
  const numAppsToGenerate = maxProjectsInProgress - inProgressProjects.length;
  const appsToGenerate = pendingProjects.slice(0, numAppsToGenerate);
  for (const app of appsToGenerate) {
    generateAppJob.submit({ appId: app.id });
  }
}
