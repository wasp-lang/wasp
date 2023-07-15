import {
  RegisterZipDownload,
  StartGeneratingNewApp,
  CreateFeedback
} from "@wasp/actions/types";
import { GetAppGenerationResult, GetStats, GetFeedback } from "@wasp/queries/types";
import HttpError from "@wasp/core/HttpError.js";
import { checkPendingAppsJob } from "@wasp/jobs/checkPendingAppsJob.js";

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
  if (!args.appName) {
    throw new HttpError(422, "App name is required.");
  }
  if (!/^[a-zA-Z_][a-zA-Z0-9_-]*$/.test(args.appName)) {
    throw new HttpError(
      422,
      "App name can only contain letters, numbers, dashes, or underscores."
    );
  }
  if (!args.appDesc) {
    throw new HttpError(422, "App description is required.");
  }
  const { Project } = context.entities;
  const optionalUser = context.user
    ? {
        user: {
          connect: {
            id: context.user.id,
          },
        },
      }
    : {};
  const project = await Project.create({
    data: {
      name: args.appName,
      description: args.appDesc,
      primaryColor: args.appPrimaryColor,
      authMethod: args.appAuthMethod,
      creativityLevel: args.appCreativityLevel,
      referrer: args.referrer,
      ...optionalUser,
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
    if (e.name === "NotFoundError") {
      throw new HttpError(404, "App not found.");
    } else { throw e; }
  }
};

export const createFeedback: CreateFeedback<
  { score: number , message: string, projectId: string }
> = (async (args, context) => {
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
      projectId: args.projectId
    }
  })

})

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
    if (e.name === "NotFoundError") {
      throw new HttpError(404, "App not found.");
    } else { throw e; }
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
        }
      }
    }
  })

  return {
    feedbackEntries
  }
}) satisfies GetFeedback<{}>;

export const getStats = (async (_args, context) => {
  const emailsWhitelist = process.env.ADMIN_EMAILS_WHITELIST?.split(",") || [];
  if (!context.user || !emailsWhitelist.includes(context.user.email)) {
    throw new HttpError(401, "Only admins can access stats.");
  }

  const { Project } = context.entities;
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
      logs: {
        orderBy: {
          createdAt: "desc",
        },
      },
    },
  });
  return {
    projects,
  };
}) satisfies GetStats<{}>;
