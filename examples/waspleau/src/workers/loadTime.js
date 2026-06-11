import ky from "ky";
import { upsertData } from "./utils.js";

export async function measureLoadTime(args, context) {
  console.log("loadTime.js measureLoadTime", args, context);

  const start = Date.now();
  await ky.get(args.url);
  const end = Date.now();

  const data = [{ name: args.name, value: `${end - start}ms` }];

  await Promise.all(data.map(upsertData(context)));

  return data;
}
