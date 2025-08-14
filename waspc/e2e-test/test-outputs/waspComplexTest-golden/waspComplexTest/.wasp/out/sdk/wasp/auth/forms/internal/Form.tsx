import { ComponentPropsWithoutRef, ComponentRef, forwardRef } from "react";
import styles from "./Form.module.css";
import "./auth-styles.css";
import { clsx } from "./util";

// PRIVATE API
export const Form = forwardRef<
  ComponentRef<"form">,
  ComponentPropsWithoutRef<"form">
>(({ children, className, ...props }) => (
  <form className={clsx(styles.form, className)} {...props}>
    {children}
  </form>
));

// PUBLIC API
export const FormItemGroup = forwardRef<
  ComponentRef<"div">,
  ComponentPropsWithoutRef<"div">
>(({ children, className, ...props }) => (
  <div className={clsx(styles.formItemGroup, className)} {...props}>
    {children}
  </div>
));

// PUBLIC API
export const FormLabel = forwardRef<
  ComponentRef<"label">,
  ComponentPropsWithoutRef<"label">
>(({ children, className, ...props }) => (
  <label className={clsx(styles.formLabel, className)} {...props}>
    {children}
  </label>
));

// PUBLIC API
export const FormInput = forwardRef<
  ComponentRef<"input">,
  ComponentPropsWithoutRef<"input">
>(({ className, ...props }) => (
  <input className={clsx(styles.formInput, className)} {...props} />
));

// PUBLIC API)
export const FormTextarea = forwardRef<
  ComponentRef<"textarea">,
  ComponentPropsWithoutRef<"textarea">
>(({ className, ...props }) => (
  <textarea className={clsx(styles.formTextarea, className)} {...props} />
));

// PUBLIC API)
export const FormError = forwardRef<
  ComponentRef<"div">,
  ComponentPropsWithoutRef<"div">
>(({ children, className, ...props }) => (
  <div className={clsx(styles.formError, className)} {...props}>
    {children}
  </div>
));

// PRIVATE API
export const SubmitButton = forwardRef<
  ComponentRef<"button">,
  ComponentPropsWithoutRef<"button">
>(({ children, className, ...props }) => (
  <button className={clsx(styles.submitButton, className)} {...props}>
    {children}
  </button>
));
