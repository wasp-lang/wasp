import { jsx as _jsx } from "react/jsx-runtime";
import { forwardRef, } from "react";
import styles from "./Message.module.css";
import "./auth-styles.css";
import { clsx } from "./util.js";
export const Message = forwardRef(({ children, className, ...props }, ref) => (_jsx("div", { className: clsx(styles.message, className), ...props, ref: ref, children: children })));
export const MessageError = forwardRef(({ children, className, ...props }, ref) => (_jsx("div", { className: clsx(styles.messageError, className), ...props, ref: ref, children: children })));
export const MessageSuccess = forwardRef(({ children, className, ...props }, ref) => (_jsx("div", { className: clsx(styles.messageSuccess, className), ...props, ref: ref, children: children })));
