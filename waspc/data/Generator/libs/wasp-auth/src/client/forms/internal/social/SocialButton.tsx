import {
  forwardRef,
  type ComponentPropsWithoutRef,
  type ComponentRef,
} from "react";
import "../auth-styles.css";
import { clsx } from "../util.js";
import styles from "./SocialButton.module.css";

export const SocialButton = forwardRef<
  ComponentRef<"a">,
  ComponentPropsWithoutRef<"a">
>(({ children, className, ...props }, ref) => (
  <a className={clsx(styles.socialButton, className)} {...props} ref={ref}>
    {children}
  </a>
));
