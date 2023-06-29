import { StartGeneratingNewApp } from "@wasp/actions/types";
import { GetAppGenerationResult } from "@wasp/queries/types";
import HttpError from "@wasp/core/HttpError.js";
import { spawn } from "child_process";
import { Mutex } from "async-mutex";

const appGenerationResults: Record<string, any> = {};

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
  const { Project, File, Log } = context.entities;
  const project = await Project.create({
    data: {
      name: args.appName,
      description: args.appDesc,
    },
  });

  const appId = project.id;
  appGenerationResults[appId] = {
    unconsumedStdout: "",
  };

  // { auth: 'UsernameAndPassword', primaryColor: string }
  const projectConfig = {
    primaryColor: args.appPrimaryColor,
  };

  const stdoutMutex = new Mutex();
  let waspCliProcess = null;
  const waspCliProcessArgs = [
    "new-ai",
    args.appName,
    args.appDesc,
    JSON.stringify(projectConfig),
  ];

  if (process.env.NODE_ENV === "production") {
    waspCliProcess = spawn("wasp", waspCliProcessArgs);
  } else {
    // NOTE: In dev when we use `wasp-cli`, we want to make sure that if this app is run via `wasp` that its datadir env var does not propagate,
    //   so we reset it here. This is problem only if you run app with `wasp` and let it call `wasp-cli` here.
    waspCliProcess = spawn("wasp-cli", waspCliProcessArgs, {
      env: { ...process.env, waspc_datadir: undefined },
    });
  }

  waspCliProcess.stdout.on("data", async (data) => {
    const release = await stdoutMutex.acquire();
    try {
      appGenerationResults[appId].unconsumedStdout += data;
      const patterns = [
        {
          regex: /==== WASP AI: LOG ====\n([\s\S]*?)\n===\/ WASP AI: LOG ====/,
          action: async (match: RegExpMatchArray) => {
            const content = match[1];
            // console.log(`Log: ${content}`);
            await Log.create({
              data: {
                content,
                project: {
                  connect: { id: appId },
                },
              },
            });
          },
        },
        {
          regex:
            /==== WASP AI: WRITE FILE ====\n([\s\S]*?)\n===\/ WASP AI: WRITE FILE ====/,
          action: async (match: RegExpMatchArray) => {
            const text = match[1];
            const [filename, ...rest] = text.split("\n");
            const content = rest.join("\n");
            const file = await File.findFirst({
              where: { name: filename, projectId: appId },
            });
            if (file) {
              // console.log(`Updating file ${filename} in project ${appId}.`);
              await File.update({
                where: { id: file.id },
                data: { content },
              });
            } else {
              // console.log(`Creating file ${filename} in project ${appId}.`);
              await File.create({
                data: {
                  name: filename,
                  content,
                  projectId: appId,
                },
              });
            }
          },
        },
      ];

      let match: any = null;
      do {
        match = null;
        for (const pattern of patterns) {
          const newMatch = pattern.regex.exec(
            appGenerationResults[appId].unconsumedStdout
          );
          if (newMatch && (!match || newMatch.index < match.index)) {
            match = {
              ...newMatch,
              action: pattern.action,
            };
          }
        }

        if (match) {
          await match.action(match);
          appGenerationResults[appId].unconsumedStdout = appGenerationResults[
            appId
          ].unconsumedStdout.replace(match[0], "");
        }
      } while (match);
    } finally {
      release();
    }
  });

  waspCliProcess.stderr.on("data", (data) => {
    console.error(data.toString());
  });

  waspCliProcess.on("close", async (code) => {
    if (code === 0) {
      await Project.update({
        where: { id: appId },
        data: { status: "success" },
      });
    } else {
      await Project.update({
        where: { id: appId },
        data: { status: "failure" },
      });
    }
  });

  waspCliProcess.on("error", async (err) => {
    console.error("WASP CLI PROCESS ERROR", err);
    await Project.update({
      where: { id: appId },
      data: { status: "failure" },
    });
  });

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
            createdAt: "desc",
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
