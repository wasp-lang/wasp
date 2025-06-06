import React from "react";
import { Link } from "react-router-dom";
import { cn } from "../cn";

type ButtonVariant =
  | "primary"
  | "secondary"
  | "outline"
  | "ghost"
  | "danger"
  | "success"
  | "warning";

interface BaseButtonProps {
  variant?: ButtonVariant;
  disabled?: boolean;
  children: React.ReactNode;
  className?: string;
}

interface RegularButtonProps extends BaseButtonProps {
  onClick?: (e: React.MouseEvent<HTMLButtonElement>) => void;
  type?: "button" | "submit" | "reset";
}

interface LinkButtonProps extends BaseButtonProps {
  to: string;
  replace?: boolean;
}

type ButtonProps = RegularButtonProps | LinkButtonProps;

function isLinkButton(props: ButtonProps): props is LinkButtonProps {
  return "to" in props;
}

const variantStyles: Record<ButtonVariant, string> = {
  primary:
    "bg-primary-500 hover:bg-primary-400 text-gray-800 border-transparent shadow-sm hover:shadow-md",
  secondary:
    "bg-gray-600 hover:bg-gray-700 text-white border-transparent shadow-sm hover:shadow-md",
  outline:
    "bg-transparent hover:bg-primary-50 text-primary-600 border-primary-400 hover:border-primary-300",
  ghost: "bg-transparent hover:bg-gray-100 text-gray-700 border-transparent",
  danger:
    "bg-red-600 hover:bg-red-700 text-white border-transparent shadow-sm hover:shadow-md",
  success:
    "bg-green-600 hover:bg-green-700 text-white border-transparent shadow-sm hover:shadow-md",
  warning:
    "bg-amber-600 hover:bg-amber-700 text-white border-transparent shadow-sm hover:shadow-md",
};

const disabledStyles =
  "opacity-50 cursor-not-allowed hover:transform-none hover:shadow-none pointer-events-none";

export function Button(props: ButtonProps) {
  const { disabled, children, variant = "primary", className = "" } = props;

  const classes = cn(
    "inline-flex items-center justify-center",
    "font-medium rounded-lg border",
    "transition-all duration-200 ease-in-out",
    "focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-primary-500",
    "select-none",
    "px-4 py-2 text-sm",
    variantStyles[variant],
    disabled && disabledStyles,
    className,
  );

  if (isLinkButton(props)) {
    const { to, replace } = props;

    return (
      <Link to={to} replace={replace} className={classes}>
        {children}
      </Link>
    );
  }

  const { onClick, type = "button" } = props;

  return (
    <button
      type={type}
      onClick={onClick}
      disabled={disabled}
      className={classes}
    >
      {children}
    </button>
  );
}
