import { createContext, useContext } from "react";
import type { ErrorMessage } from "./messages";

export type AuthContextValue = {
  isLoading: boolean;
  setIsLoading: (isLoading: boolean) => void;
  setErrorMessage: (errorMessage: ErrorMessage | null) => void;
  setSuccessMessage: (successMessage: string | null) => void;
};

export const AuthContext = createContext<AuthContextValue>({
  isLoading: false,
  setIsLoading: () => {},
  setErrorMessage: () => {},
  setSuccessMessage: () => {},
});

export const useAuthContext = () => {
  const context = useContext(AuthContext);
  if (!context) {
    throw new Error("useAuthContext must be used within an AuthProvider");
  }
  return context;
};
