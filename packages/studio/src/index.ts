import express from "express";
import morgan from "morgan";

// The Wasp Studio page reads the app's spec straight from the server that
// `wasp start` runs, so this only serves the page.
const app = express();

app.use(morgan("dev"));

const publicDirPath = new URL("./public", import.meta.url).pathname;
app.use(express.static(publicDirPath));

app.listen(4000, () => {
  console.log("Server listening on port 4000");
});
