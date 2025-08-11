import { createContext, useContext } from "react";

export type ErrorMessage = {
  title: string;
  description?: string;
};

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
