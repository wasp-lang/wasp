import { ComponentPropsWithoutRef, ComponentRef, forwardRef } from "react";
import "../../../../../core/auth/forms/internal/auth-styles.css";
import { clsx } from "../../../../../core/auth/forms/internal/util.js";
import styles from "./SocialButton.module.css";

// PRIVATE API
export const SocialButton = forwardRef<
  ComponentRef<"a">,
  ComponentPropsWithoutRef<"a">
>(({ children, className, ...props }, ref) => (
  <a className={clsx(styles.socialButton, className)} {...props} ref={ref}>
    {children}
  </a>
));
