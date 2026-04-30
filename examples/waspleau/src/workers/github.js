import ky from "ky";
import { upsertData } from "./utils.js";

export async function workerFunction(args, context) {
  console.log("github.js workerFunction", args, context);

  const response = await ky
    .get("https://api.github.com/repos/wasp-lang/wasp")
    .json();

  const data = [
    { name: "Wasp GitHub Stars", value: response.stargazers_count },
    { name: "Wasp GitHub Language", value: response.language },
    { name: "Wasp GitHub Forks", value: response.forks },
    { name: "Wasp GitHub Open Issues", value: response.open_issues },
  ];

  await Promise.all(data.map(upsertData(context)));

  return data;
}
