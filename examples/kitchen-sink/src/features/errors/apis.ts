import { HttpError } from "wasp/errors";
import {
  type ThrowHttpError,
  type ThrowUnexpectedError,
} from "wasp/server/api";

export const throwUnexpectedError: ThrowUnexpectedError = async () => {
  throw new Error("Internal detail that must not reach the client");
};

export const throwHttpError: ThrowHttpError = async () => {
  throw new HttpError(418, "I'm a teapot", {
    data: { reason: "short and stout" },
    headers: { "Content-Type": "application/vnd.teapot+json" },
  });
};
