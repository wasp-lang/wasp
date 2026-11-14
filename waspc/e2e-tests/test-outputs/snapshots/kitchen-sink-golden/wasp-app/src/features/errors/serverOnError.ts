import { getErrorMessage } from "wasp/errors";
import { type ServerOnErrorFn } from "wasp/server";
import { type GetReportedServerErrors } from "wasp/server/api";

type ServerErrorReport = { source: string; message: string };

const MAX_SERVER_ERROR_REPORTS = 100;
const serverErrorReports: ServerErrorReport[] = [];

export const onServerError: ServerOnErrorFn = (error, context) => {
  serverErrorReports.push({
    source: context.source,
    message: getErrorMessage(error),
  });
  if (serverErrorReports.length > MAX_SERVER_ERROR_REPORTS) {
    serverErrorReports.shift();
  }
};

export const getReportedServerErrors: GetReportedServerErrors = async (
  _req,
  res,
) => {
  res.json(serverErrorReports);
};
