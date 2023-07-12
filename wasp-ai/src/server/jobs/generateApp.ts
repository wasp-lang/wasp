import { spawn } from "child_process";
import { Mutex } from "async-mutex";
import { log } from "./utils.js";

const appGenerationResults: Record<string, any> = {};

export async function generateApp(
  args: { appId: string },
  context: {
    entities: {
      Project: any;
      Log: any;
      File: any;
    };
  }
) {
  log("Generating app");
  const appId = args.appId;

  const { Project, Log, File } = context.entities;

  const project = await Project.findUniqueOrThrow({
    where: { id: appId },
  });

  if (project.status !== "pending") {
    return {
      success: false,
    };
  }

  await Project.update({
    where: { id: appId },
    data: { status: "in-progress" },
  });

  appGenerationResults[appId] = {
    unconsumedStdout: "",
  };

  let defaultGptTemperature;
  switch (project.creativityLevel) {
    case "conservative":
      defaultGptTemperature = 0.4;
      break;
    case "balanced":
      defaultGptTemperature = 0.7;
      break;
    case "creative":
      defaultGptTemperature = 1.0;
      break;
    default:
      throw new Error(`Unknown creativity level: ${project.creativityLevel}`);
  };

  // { auth: 'UsernameAndPassword', primaryColor: string, defaultGptTemperature: number }
  const projectConfig = {
    primaryColor: project.primaryColor,
    defaultGptTemperature
  };

  const stdoutMutex = new Mutex();
  let waspCliProcess = null;
  const waspCliProcessArgs = [
    "new-ai",
    project.name,
    project.description,
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

  return { success: true };
}
