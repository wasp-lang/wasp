import { HttpError } from "wasp/errors";
import { type GetTeapot } from "wasp/server/operations";

export const getTeapot: GetTeapot<void, never> = async () => {
  throw new HttpError(418, "I'm a teapot", { reason: "short and stout" });
};
