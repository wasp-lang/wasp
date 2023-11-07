import { getNowInUTC } from "../utils.js";
import { log } from "./utils.js";
import type { FailStaleAppsJobs } from "@wasp/jobs/failStaleAppsJobs.js";

export const failStaleGenerations: FailStaleAppsJobs<
  {},
  {
    success: boolean;
  }
> = async (_args, context) => {
  log("Failing stale generations");
  const { Project, Log } = context.entities;

  const now = getNowInUTC();
  const fiveMinutesAgo = new Date(now.getTime() - 10 * 60 * 1000);

  try {
    const staleProjects = await Project.findMany({
      where: {
        status: "in-progress",
        logs: {
          every: {
            createdAt: {
              lte: fiveMinutesAgo,
            },
          },
        },
      },
      include: {
        logs: {
          select: {
            id: true,
          },
        },
      },
    });

    for (const project of staleProjects) {
      if (project.logs.length === 0) {
        continue;
      }
      await Project.update({
        where: { id: project.id },
        data: { status: "cancelled" },
      });
      await Log.create({
        data: {
          project: { connect: { id: project.id } },
          content: "The generation took too long.",
        },
      });
    }
    return {
      success: true,
    };
  } catch (e) {
    console.log("Error fetching projects:", e);
    return {
      success: false,
    };
  }
};
