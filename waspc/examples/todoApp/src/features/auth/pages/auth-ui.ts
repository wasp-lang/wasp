import todoLogo from "../../../assets/todoLogo.png";

export const customisationProps = {
  appearance: {
    colors: {
      brand: "#5969b8",
      brandAccent: "#de5998",
      submitButtonText: "white",
    },
  },
  logo: todoLogo,
  socialLayout: "horizontal",
} as const;
