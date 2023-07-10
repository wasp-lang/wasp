import { StartGeneratingNewApp } from "@wasp/actions/types";
import { GetAppGenerationResult, GetStats } from "@wasp/queries/types";
import HttpError from "@wasp/core/HttpError.js";
import { checkPendingAppsJob } from "@wasp/jobs/checkPendingAppsJob.js";

export const startGeneratingNewApp: StartGeneratingNewApp<
  {
    appName: string;
    appDesc: string;
    appPrimaryColor: string;
    appAuthMethod: string;
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
      ...optionalUser,
    },
  });

  const appId = project.id;

  checkPendingAppsJob.submit({});

  return appId;
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
    return {
      project,
    };
  } catch (e) {
    throw new HttpError(404, "App not found.");
  }
}) satisfies GetAppGenerationResult<{
  appId: string;
}>;

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
