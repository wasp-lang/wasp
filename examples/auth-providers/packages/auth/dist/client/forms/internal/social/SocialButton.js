import { jsx as _jsx } from "react/jsx-runtime";
import { forwardRef, } from "react";
import "../auth-styles.css";
import { clsx } from "../util.js";
import styles from "./SocialButton.module.css";
export const SocialButton = forwardRef(({ children, className, ...props }, ref) => (_jsx("a", { className: clsx(styles.socialButton, className), ...props, ref: ref, children: children })));
