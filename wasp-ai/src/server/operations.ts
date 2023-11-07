import { RegisterZipDownload, StartGeneratingNewApp, CreateFeedback } from "@wasp/actions/types";
import { GetAppGenerationResult, GetStats, GetFeedback, GetNumProjects, GetProjectsByUser } from "@wasp/queries/types";
import HttpError from "@wasp/core/HttpError.js";
import { checkPendingAppsJob } from "@wasp/jobs/checkPendingAppsJob.js";
import { getNowInUTC } from "./utils.js";
import type { Project } from "@wasp/entities";

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

export const getStats = (async (_args, context) => {
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

  // All projects but without logs
  const projects = await Project.findMany({
    orderBy: {
      createdAt: "desc",
    },
    include: {
      user: {
        select: {
          email: true,
        },
      },
    },
  });

  return {
    projects,
    latestProjectsWithLogs,
  };
}) satisfies GetStats<{}>;

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
