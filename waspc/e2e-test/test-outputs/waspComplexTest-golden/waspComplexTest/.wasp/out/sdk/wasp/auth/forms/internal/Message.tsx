import { ComponentPropsWithoutRef, ComponentRef, forwardRef } from "react";
import styles from "./Message.module.css";
import "./auth-styles.css";
import { clsx } from "./util";

// PRIVATE API
export const Message = forwardRef<
  ComponentRef<"div">,
  ComponentPropsWithoutRef<"div">
>(({ children, className, ...props }, ref) => (
  <div className={clsx(styles.message, className)} {...props} ref={ref}>
    {children}
  </div>
));

// PRIVATE API
export const MessageError = forwardRef<
  ComponentRef<"div">,
  ComponentPropsWithoutRef<"div">
>(({ children, className, ...props }, ref) => (
  <div className={clsx(styles.messageError, className)} {...props} ref={ref}>
    {children}
  </div>
));

// PRIVATE API
export const MessageSuccess = forwardRef<
  ComponentRef<"div">,
  ComponentPropsWithoutRef<"div">
>(({ children, className, ...props }, ref) => (
  <div className={clsx(styles.messageSuccess, className)} {...props} ref={ref}>
    {children}
  </div>
));
