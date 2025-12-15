import { createContext, useContext } from "react";

/**
 * Used to display error messages in auth components.
 */
export type ErrorMessage = {
  title: string;
  description?: string;
};

/**
 * Context used across different auth components to share loading and message state.
 */
export const AuthContext = createContext({
  isLoading: false,
  setIsLoading: (isLoading: boolean) => {},
  setErrorMessage: (errorMessage: ErrorMessage | null) => {},
  setSuccessMessage: (successMessage: string | null) => {},
});

export const useAuthContext = () => {
  const context = useContext(AuthContext);
  if (!context) {
    throw new Error("useAuthContext must be used within an AuthProvider");
  }
  return context;
};

console.log("Libs doing the real talk #3");
