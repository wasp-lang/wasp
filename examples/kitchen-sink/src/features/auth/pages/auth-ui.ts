import { CustomizationOptions } from "@wasp.sh/auth/client";

export const customisationProps = {
  appearance: {
    colors: {
      brand: "#f5c842",
      brandAccent: "#fbce48",
      submitButtonText: "#111111",
    },
  },
  socialLayout: "horizontal",
} satisfies CustomizationOptions;
