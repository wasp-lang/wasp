import * as fs from "fs";

import { Command } from "commander";
import cors from "cors";
import express from "express";
import { createServer } from "http";
import morgan from "morgan";
import { Server } from "socket.io";

function getUrlFromRelativePathToCwd(path: string) {
  return new URL(path, `file://${process.cwd()}/`);
}

const program = new Command();
program
  .requiredOption("-d, --data-file <path>", "Path to data file")
  .parse(process.argv);

const options = program.opts<{
  dataFile: string;
}>();

const app = express();
const server = createServer(app);
const io = new Server(server, {
  cors: {
    origin: "*",
  },
});

const requestLogger = morgan("dev");
app.use(requestLogger);

app.use(
  cors({
    origin: "*",
  }),
);

const publicDirPath = new URL("./public", import.meta.url).pathname;
app.use(express.static(publicDirPath));

const pathToDataFile = getUrlFromRelativePathToCwd(options.dataFile);
function readFile() {
  return fs.readFileSync(pathToDataFile, "utf8");
}

let data = readFile();
fs.watch(pathToDataFile, () => {
  data = readFile();
  io.emit("data", data);
});

io.on("connection", (socket) => {
  console.log("Client connected");
  socket.emit("data", data);

  socket.on("disconnect", () => {
    console.log("Client disconnected");
  });
});

server.listen(4000, () => {
  console.log("Server listening on port 4000");
});
