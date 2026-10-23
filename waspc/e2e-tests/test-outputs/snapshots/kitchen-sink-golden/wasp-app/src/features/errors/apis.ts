import { HttpError } from "wasp/server";
import {
  type ThrowConcealedError,
  type ThrowHttpError,
  type ThrowRateLimitError,
  type ThrowUnavailableError,
  type ThrowUnexpectedError,
} from "wasp/server/api";

export const throwUnexpectedError: ThrowUnexpectedError = async () => {
  throw new Error("Internal detail that must not reach the client");
};

export const throwHttpError: ThrowHttpError = async () => {
  throw new HttpError(418, "I'm a teapot", { reason: "short and stout" });
};

// The errors below have the shape that Express middleware create with the
// `http-errors` package: https://github.com/jshttp/http-errors#error-properties

export const throwConcealedError: ThrowConcealedError = async () => {
  throw Object.assign(new Error("Internal detail of a 401"), {
    status: 401,
    statusCode: 401,
    expose: false,
  });
};

export const throwRateLimitError: ThrowRateLimitError = async () => {
  throw Object.assign(new Error("Too many requests, slow down"), {
    status: 429,
    statusCode: 429,
    expose: true,
    headers: { "Retry-After": "120" },
  });
};

// An error that is neither an `HttpError` nor an `http-errors` error, but
// still carries a status code and headers.
export const throwUnavailableError: ThrowUnavailableError = async () => {
  throw Object.assign(new Error("Internal detail of a 503"), {
    status: 503,
    headers: {
      "Retry-After": 60,
      "Set-Cookie": ["first=1; Path=/", "second=2; Path=/"],
    },
  });
};
