import { useMemo } from "react";
import { useLocation } from "react-router";
import { routes } from "wasp/client/router";

export const useIsLandingPage = () => {
  const location = useLocation();

  return useMemo(() => {
    return location.pathname === routes.LandingPageRoute.to;
  }, [location]);
};
