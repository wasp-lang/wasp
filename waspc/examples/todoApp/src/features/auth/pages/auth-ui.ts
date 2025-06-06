import { CustomizationOptions } from "wasp/client/auth";

export const customisationProps = {
  appearance: {
    colors: {
      brand: "#ffcc00",
      brandAccent: "#ffdb0d",
      submitButtonText: "#1f2937",
    },
  },
  socialLayout: "horizontal",
} satisfies CustomizationOptions;
