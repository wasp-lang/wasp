import { action, page, query, route, type Spec } from "@wasp.sh/spec";

import { FileUploadPage } from "./FileUploadPage" with { type: "ref" };
import {
  addFileToDb,
  createFileUploadUrl,
  deleteFile,
  getAllFilesByUser,
  getDownloadFileSignedURL,
} from "./operations" with { type: "ref" };

export const fileUploadSpec: Spec = [
  route(
    "FileUploadRoute",
    "/file-upload",
    page(FileUploadPage, { authRequired: true }),
  ),
  query(getAllFilesByUser, { entities: ["User", "File"] }),
  query(getDownloadFileSignedURL, { entities: ["User", "File"] }),
  action(addFileToDb, { entities: ["User", "File"] }),
  action(createFileUploadUrl, { entities: ["User", "File"] }),
  action(deleteFile, { entities: ["User", "File"] }),
];
