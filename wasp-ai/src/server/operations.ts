import { StartGeneratingNewApp } from "@wasp/actions/types";
import { GetAppGenerationResult } from "@wasp/queries/types";
import HttpError from "@wasp/core/HttpError.js";
import { spawn } from "child_process";

const appGenerationResults: Record<string, any> = {};

export const startGeneratingNewApp: StartGeneratingNewApp<
  {
    appName: string;
    appDesc: string;
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
  const { Project, File } = context.entities;
  const project = await Project.create({
    data: {
      name: args.appName,
      description: args.appDesc,
    },
  });

  const appId = project.id;
  appGenerationResults[appId] = {
    status: "in-progress",
    messages: [],
    unconsumedStdout: "",
  };

  let waspCliProcess = null;
  if (process.env.NODE_ENV === "production") {
    waspCliProcess = spawn("wasp", [
      "new-ai",
      args.appName,
      args.appDesc,
    ]);
  } else {
    // NOTE: In dev when we use `wasp-cli`, we want to make sure that if this app is run via `wasp` that its datadir env var does not propagate,
    //   so we reset it here. This is problem only if you run app with `wasp` and let it call `wasp-cli` here.
    waspCliProcess = spawn(
      "wasp-cli",
      ["new-ai", args.appName, args.appDesc],
      { env: { ...process.env, waspc_datadir: undefined } }
    );
  }

  waspCliProcess.stdout.on("data", async (data) => {
    console.log(data.toString());
    const newStdoutChunk = data.toString();
    let unconsumedStdout =
      appGenerationResults[appId].unconsumedStdout + newStdoutChunk;
    let newMessages: Message[] = [];
    while (true) {
      const firstMsgEndHeaderMatch = unconsumedStdout.match(
        /===\/ WASP AI: (LOG|WRITE FILE) ====/
      );
      if (firstMsgEndHeaderMatch) {
        const msgEndHeaderStartIdx = firstMsgEndHeaderMatch.index;
        const msgEndHeader = firstMsgEndHeaderMatch[0];
        const msgStartHeader = msgEndHeader.replace("===/", "====") + "\n";
        const msgStartHeaderMatch = unconsumedStdout.match(
          new RegExp(msgStartHeader)
        );
        const msgStartHeaderStartIdx = msgStartHeaderMatch.index;
        const message: Message = {
          text: unconsumedStdout.substring(
            msgStartHeaderStartIdx + msgStartHeader.length,
            msgEndHeaderStartIdx
          ),
          type:
            msgStartHeader === "==== WASP AI: LOG ====\n"
              ? "log"
              : "write-file",
        };
        if (message.type === "write-file") {
          const [filename, ...rest] = message.text.split("\n");
          const content = rest.join("\n");
          const file = await File.findFirst({
            where: { name: filename, projectId: appId },
          });
          if (file) {
            await File.update({
              where: { id: file.id },
              data: { content },
            });
          } else {
            await File.create({
              data: {
                name: filename,
                content,
                projectId: appId,
              },
            });
          }
        } else {
          newMessages = [...newMessages, message];
        }
        unconsumedStdout = unconsumedStdout.substring(
          msgEndHeaderStartIdx + msgEndHeader.length
        );
      } else {
        break;
      }
    }
    appGenerationResults[appId].messages = [
      ...appGenerationResults[appId].messages,
      ...newMessages,
    ];

    // TODO: Rethink logs so they are fully saved in the DB
    // await Promise.all(
    //   newMessages.map((message) =>
    //     Log.create({
    //       data: {
    //         content: message.text,
    //         project: {
    //           connect: { id: appId },
    //         },
    //       },
    //     })
    //   )
    // );
    appGenerationResults[appId].unconsumedStdout = unconsumedStdout;
  });

  waspCliProcess.stderr.on("data", (data) => {
    console.error(data.toString());
  });

  waspCliProcess.on("close", (code) => {
    console.log("WASP CLI PROCESS STOPPED");
    if (code === 0) {
      appGenerationResults[appId].status = "success";
    } else {
      appGenerationResults[appId].status = "failure";
    }
  });

  waspCliProcess.on("error", (err) => {
    console.error("WASP CLI PROCESS ERROR", err);
    appGenerationResults[appId].status = "failure";
  });

  return appId;
};

export const getAppGenerationResult = (async (args, context) => {
  const appId = args.appId;
  const { Project } = context.entities;
  const project = await Project.findUnique({
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
    status: appGenerationResults[appId]?.status ?? "success",
    messages: appGenerationResults[appId]?.messages ?? [],
    project,
  };
}) satisfies GetAppGenerationResult<{
  appId: string;
}>;

type Message = {
  text: string;
  type: "log" | "write-file";
};
