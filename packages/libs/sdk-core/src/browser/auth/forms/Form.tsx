import * as React from "react";
import styles from "./Form.module.css";
import "./auth-styles.css";
import { clsx } from "./util";

export const Form = React.forwardRef<
  React.ComponentRef<"form">,
  React.ComponentPropsWithoutRef<"form">
>(({ children, className, ...props }, ref) => (
  <form className={clsx(styles.form, className)} {...props} ref={ref}>
    {children}
  </form>
));

export const FormItemGroup = React.forwardRef<
  React.ComponentRef<"div">,
  React.ComponentPropsWithoutRef<"div">
>(({ children, className, ...props }, ref) => (
  <div className={clsx(styles.formItemGroup, className)} {...props} ref={ref}>
    {children}
  </div>
));

export const FormLabel = React.forwardRef<
  React.ComponentRef<"label">,
  React.ComponentPropsWithoutRef<"label">
>(({ children, className, ...props }, ref) => (
  <label className={clsx(styles.formLabel, className)} {...props} ref={ref}>
    {children}
  </label>
));

export const FormInput = React.forwardRef<
  React.ComponentRef<"input">,
  React.ComponentPropsWithoutRef<"input">
>(({ className, ...props }, ref) => (
  <input className={clsx(styles.formInput, className)} {...props} ref={ref} />
));

export const FormTextarea = React.forwardRef<
  React.ComponentRef<"textarea">,
  React.ComponentPropsWithoutRef<"textarea">
>(({ className, ...props }, ref) => (
  <textarea
    className={clsx(styles.formTextarea, className)}
    {...props}
    ref={ref}
  />
));

export const FormError = React.forwardRef<
  React.ComponentRef<"div">,
  React.ComponentPropsWithoutRef<"div">
>(({ children, className, ...props }, ref) => (
  <div className={clsx(styles.formError, className)} {...props} ref={ref}>
    {children}
  </div>
));

export const SubmitButton = React.forwardRef<
  React.ComponentRef<"button">,
  React.ComponentPropsWithoutRef<"button">
>(({ children, className, ...props }, ref) => (
  <button className={clsx(styles.submitButton, className)} {...props} ref={ref}>
    {children}
  </button>
));
