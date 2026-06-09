import { action, page, query, route, type Spec } from "@wasp.sh/spec";

import { AccountPage } from "./AccountPage" with { type: "ref" };
import {
  getPaginatedUsers,
  updateIsUserAdminById,
} from "./operations" with { type: "ref" };

export const userSpec: Spec = [
  route("AccountRoute", "/account", page(AccountPage, { authRequired: true })),
  query(getPaginatedUsers, { entities: ["User"] }),
  action(updateIsUserAdminById, { entities: ["User"] }),
];
