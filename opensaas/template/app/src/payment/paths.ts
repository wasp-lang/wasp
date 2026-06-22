import { routes } from "wasp/client/router";
import { config } from "wasp/server";
import { CheckoutResult } from "./CheckoutResultPage";

const CHECKOUT_SUCCESS_URL_PATH = routes.CheckoutResultRoute.build({
  search: {
    status: CheckoutResult.Success,
  },
});
export const CHECKOUT_SUCCESS_URL = `${config.frontendUrl}${CHECKOUT_SUCCESS_URL_PATH}`;

const CHECKOUT_CANCELED_URL_PATH = routes.CheckoutResultRoute.build({
  search: {
    status: CheckoutResult.Canceled,
  },
});
export const CHECKOUT_CANCELED_URL = `${config.frontendUrl}${CHECKOUT_CANCELED_URL_PATH}`;

export const CUSTOMER_PORTAL_RETURN_URL = `${config.frontendUrl}${routes.AccountRoute.to}`;
