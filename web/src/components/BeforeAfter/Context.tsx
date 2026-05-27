import { ComponentType, createContext, ReactNode, useContext } from "react";

export interface BeforeAfterValue {
  sideBySide: boolean;
}

const BeforeAfterContext = createContext<BeforeAfterValue | null>(null);
BeforeAfterContext.displayName = "BeforeAfterContext";

export const BeforeAfterProvider: ComponentType<{
  value: BeforeAfterValue;
  children?: ReactNode;
}> = BeforeAfterContext.Provider;

export const useBeforeAfterContext = () => {
  const context = useContext(BeforeAfterContext);
  if (!context) {
    throw new Error(
      "useBeforeAfterContext must be used within a BeforeAfterProvider",
    );
  }
  return context;
};
