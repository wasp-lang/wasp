import { HttpError } from "wasp/errors";
import { type GetTeapot } from "wasp/server/operations";

export const getTeapot: GetTeapot<void, never> = async () => {
  throw new HttpError(418, {
    message: "I'm a teapot",
    data: { reason: "short and stout" },
  });
};
