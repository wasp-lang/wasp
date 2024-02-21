import { checkPendingAppsJob } from "wasp/server/jobs";
import { type Project, type User } from "wasp/entities";
import { HttpError } from "wasp/server";

import {
  type RegisterZipDownload,
  type StartGeneratingNewApp,
  type CreateFeedback,
  type DeleteMyself,
  type GetAppGenerationResult,
  type GetStats,
  type GetProjects,
  type GetFeedback,
  type GetNumProjects,
  type GetProjectsByUser,
} from "wasp/server/operations";

import { getNowInUTC } from "./utils.js";
import type { Prisma } from "@prisma/client";
import { generateLast24HoursData, generateLast30DaysData } from "./stats.js";

export const startGeneratingNewApp: StartGeneratingNewApp<
  {
    referrer: string;
    appName: string;
    appDesc: string;
    appPrimaryColor: string;
    appAuthMethod: string;
    appCreativityLevel: string;
  },
  string
> = async (args, context) => {
  if (!context.user) {
    throw new HttpError(401, "Not authorized.");
  }
  if (!args.appName) {
    throw new HttpError(422, "App name is required.");
  }
  if (!/^[a-zA-Z_][a-zA-Z0-9_-]*$/.test(args.appName)) {
    throw new HttpError(422, "App name can only contain letters, numbers, dashes, or underscores.");
  }
  if (!args.appDesc) {
    throw new HttpError(422, "App description is required.");
  }
  const { Project } = context.entities;

  const project = await Project.create({
    data: {
      name: args.appName,
      description: args.appDesc,
      primaryColor: args.appPrimaryColor,
      authMethod: args.appAuthMethod,
      creativityLevel: args.appCreativityLevel,
      referrer: args.referrer,
      user: {
        connect: {
          id: context.user.id,
        },
      },
    },
  });

  const appId = project.id;

  checkPendingAppsJob.submit({});

  return appId;
};

export const registerZipDownload: RegisterZipDownload<{
  appId: string;
}> = async (args, context) => {
  const appId = args.appId;
  try {
    await context.entities.Project.update({
      where: { id: appId },
      data: {
        zipDownloadedAt: new Date(),
      },
    });
  } catch (e) {
    if ((e as any).name === "NotFoundError") {
      throw new HttpError(404, "App not found.");
    } else {
      throw e;
    }
  }
};

export const createFeedback: CreateFeedback<{
  score: number;
  message: string;
  projectId: string;
}> = async (args, context) => {
  if (!args.score) {
    throw new HttpError(422, "Score is required.");
  }
  if (!args.message) {
    throw new HttpError(422, "Message is required.");
  }

  const feedback = await context.entities.Feedback.create({
    data: {
      score: args.score,
      message: args.message,
      projectId: args.projectId,
    },
  });
};

export const getAppGenerationResult = (async (args, context) => {
  const appId = args.appId;
  const { Project } = context.entities;
  try {
    const project = await Project.findUniqueOrThrow({
      where: { id: appId },
      include: {
        files: true,
        logs: {
          orderBy: {
            createdAt: "asc",
          },
        },
      },
    });

    const numberOfProjectsAheadInQueue =
      (await Project.count({
        where: {
          createdAt: {
            lt: project.createdAt,
          },
          status: "pending",
        },
      })) + 1;

    return {
      project,
      numberOfProjectsAheadInQueue,
    };
  } catch (e) {
    if ((e as any).name === "NotFoundError") {
      throw new HttpError(404, "App not found.");
    } else {
      throw e;
    }
  }
}) satisfies GetAppGenerationResult<{
  appId: string;
}>;

export const getFeedback = (async (args, context) => {
  // TODO(matija): extract this, since it's used at multiple locations?
  const emailsWhitelist = process.env.ADMIN_EMAILS_WHITELIST?.split(",") || [];
  if (!context.user || !emailsWhitelist.includes(context.user.email)) {
    throw new HttpError(401, "Only admins can access stats.");
  }

  const feedbackEntries = await context.entities.Feedback.findMany({
    orderBy: {
      createdAt: "desc",
    },
    include: {
      project: {
        select: {
          name: true,
          description: true,
        },
      },
    },
  });

  return {
    feedbackEntries,
  };
}) satisfies GetFeedback<{}>;

export const getProjects = (async (_args, context) => {
  const emailsWhitelist = process.env.ADMIN_EMAILS_WHITELIST?.split(",") || [];
  if (!context.user || !emailsWhitelist.includes(context.user.email)) {
    throw new HttpError(401, "Only admins can access stats.");
  }

  const { Project } = context.entities;

  const now = getNowInUTC();
  const nowMinus24Hours = new Date(now.getTime() - 24 * 60 * 60 * 1000);

  // Projects created in the last 24 hours, with logs
  const latestProjectsWithLogs = await Project.findMany({
    where: {
      createdAt: {
        gte: nowMinus24Hours,
      },
    },
    include: {
      logs: {
        orderBy: {
          createdAt: "desc",
        },
      },
    },
  });

  // Latest 1000 projects but without logs
  const projects = await Project.findMany({
    orderBy: {
      createdAt: "desc",
    },
    // Excluding description since we are not showing it in the table.
    select: {
      id: true,
      name: true,
      primaryColor: true,
      authMethod: true,
      creativityLevel: true,
      createdAt: true,
      status: true,
      referrer: true,
      zipDownloadedAt: true,
      user: {
        select: {
          email: true,
        },
      },
    },
    take: 1000,
  });

  return {
    projects,
    latestProjectsWithLogs,
  };
}) satisfies GetProjects<{}>;

export const getStats = (async (args, context) => {
  const emailsWhitelist = process.env.ADMIN_EMAILS_WHITELIST?.split(",") || [];
  if (!context.user || !emailsWhitelist.includes(context.user.email)) {
    throw new HttpError(401, "Only admins can access stats.");
  }

  const { Project } = context.entities;

  const filterOutExampleAppsCondition = args.filterOutExampleApps
    ? ({
        name: {
          not: {
            in: ["TodoApp", "MyPlants", "Blog"],
          },
        },
      } satisfies Prisma.ProjectWhereInput)
    : {};

  const projectsAfterDownloadTrackingCondition = {
    createdAt: {
      gt: new Date("2023-07-14 10:36:45.12"),
    },
    status: "success",
  };
  const [totalGenerated, projectsAfterDownloadTracking, downloadedProjects, last30DaysProjects] =
    await Promise.all([
      Project.count({
        where: {
          ...filterOutExampleAppsCondition,
        },
      }),
      Project.count({
        where: {
          ...projectsAfterDownloadTrackingCondition,
          ...filterOutExampleAppsCondition,
        },
      }),
      Project.count({
        where: {
          ...projectsAfterDownloadTrackingCondition,
          ...filterOutExampleAppsCondition,
          zipDownloadedAt: {
            not: null,
          },
        },
      }),
      Project.findMany({
        where: {
          createdAt: {
            gte: new Date(new Date().getTime() - 30 * 24 * 60 * 60 * 1000),
          },
          ...filterOutExampleAppsCondition,
        },
        select: {
          createdAt: true,
        },
      }),
    ]);
  const downloadRatio =
    projectsAfterDownloadTracking > 0 ? downloadedProjects / projectsAfterDownloadTracking : 0;

  return {
    totalGenerated,
    totalDownloaded: downloadedProjects,
    downloadedPercentage: Math.round(downloadRatio * 10000) / 100,
    last24Hours: generateLast24HoursData(last30DaysProjects),
    last30Days: generateLast30DaysData(last30DaysProjects),
  };
}) satisfies GetStats<{
  filterOutExampleApps: boolean;
}>;

export const getNumProjects = (async (_args, context) => {
  return context.entities.Project.count();
}) satisfies GetNumProjects<{}>;

export const getProjectsByUser: GetProjectsByUser<void, Project[]> = async (_args, context) => {
  if (!context.user) {
    throw new HttpError(401, "Not authorized");
  }
  return await context.entities.Project.findMany({
    where: {
      user: {
        id: context.user.id,
      },
    },
    orderBy: {
      createdAt: "desc",
    },
  });
};

export const deleteMyself: DeleteMyself<void, User> = async (args, context) => {
  if (!context.user) {
    throw new HttpError(401, "Not authorized");
  }
  try {
    await context.entities.Log.deleteMany({
      where: {
        project: {
          user: {
            id: context.user.id,
          },
        },
      },
    });
    await context.entities.File.deleteMany({
      where: {
        project: {
          user: {
            id: context.user.id,
          },
        },
      },
    });
    await context.entities.Project.updateMany({
      where: {
        user: {
          id: context.user.id,
        },
      },
      data: {
        zipDownloadedAt: undefined,
        name: "Deleted project",
        description: "Deleted project",
        status: "deleted",
      },
    });
    return await context.entities.User.delete({
      where: {
        id: context.user.id,
      },
    });
  } catch (error) {
    console.error(error);
    throw new HttpError(500, "Error deleting user");
  }
};
