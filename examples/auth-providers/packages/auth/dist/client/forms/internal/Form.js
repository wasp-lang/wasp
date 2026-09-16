import { jsx as _jsx } from "react/jsx-runtime";
import { forwardRef, } from "react";
import styles from "./Form.module.css";
import "./auth-styles.css";
import { clsx } from "./util.js";
export const Form = forwardRef(({ children, className, ...props }, ref) => (_jsx("form", { className: clsx(styles.form, className), ...props, ref: ref, children: children })));
// PUBLIC API
export const FormItemGroup = forwardRef(({ children, className, ...props }, ref) => (_jsx("div", { className: clsx(styles.formItemGroup, className), ...props, ref: ref, children: children })));
// PUBLIC API
export const FormLabel = forwardRef(({ children, className, ...props }, ref) => (_jsx("label", { className: clsx(styles.formLabel, className), ...props, ref: ref, children: children })));
// PUBLIC API
export const FormInput = forwardRef(({ className, ...props }, ref) => (_jsx("input", { className: clsx(styles.formInput, className), ...props, ref: ref })));
// PUBLIC API
export const FormTextarea = forwardRef(({ className, ...props }, ref) => (_jsx("textarea", { className: clsx(styles.formTextarea, className), ...props, ref: ref })));
// PUBLIC API
export const FormError = forwardRef(({ children, className, ...props }, ref) => (_jsx("div", { className: clsx(styles.formError, className), ...props, ref: ref, children: children })));
// PUBLIC API
export const SubmitButton = forwardRef(({ children, className, ...props }, ref) => (_jsx("button", { className: clsx(styles.submitButton, className), ...props, ref: ref, children: children })));
