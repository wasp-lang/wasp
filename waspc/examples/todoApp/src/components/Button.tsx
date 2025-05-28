import React from "react";
import { Link } from "react-router-dom";

type ButtonVariant =
  | "primary"
  | "secondary"
  | "outline"
  | "ghost"
  | "danger"
  | "success"
  | "warning";

type ButtonSize = "sm" | "md" | "lg" | "xl";

interface BaseButtonProps {
  variant?: ButtonVariant;
  size?: ButtonSize;
  fullWidth?: boolean;
  disabled?: boolean;
  loading?: boolean;
  leftIcon?: React.ReactNode;
  rightIcon?: React.ReactNode;
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
    "bg-indigo-600 hover:bg-indigo-700 text-white border-transparent shadow-sm hover:shadow-md",
  secondary:
    "bg-gray-600 hover:bg-gray-700 text-white border-transparent shadow-sm hover:shadow-md",
  outline:
    "bg-transparent hover:bg-indigo-50 text-indigo-600 border-indigo-300 hover:border-indigo-400",
  ghost: "bg-transparent hover:bg-gray-100 text-gray-700 border-transparent",
  danger:
    "bg-red-600 hover:bg-red-700 text-white border-transparent shadow-sm hover:shadow-md",
  success:
    "bg-green-600 hover:bg-green-700 text-white border-transparent shadow-sm hover:shadow-md",
  warning:
    "bg-amber-600 hover:bg-amber-700 text-white border-transparent shadow-sm hover:shadow-md",
};

const sizeStyles: Record<ButtonSize, string> = {
  sm: "px-3 py-1.5 text-sm",
  md: "px-4 py-2 text-sm",
  lg: "px-6 py-3 text-base",
  xl: "px-8 py-4 text-lg",
};

const disabledStyles =
  "opacity-50 cursor-not-allowed hover:transform-none hover:shadow-none";
const loadingStyles = "cursor-wait";

function getButtonClasses(props: ButtonProps): string {
  const {
    variant = "primary",
    size = "md",
    fullWidth = false,
    disabled = false,
    loading = false,
    className = "",
  } = props;

  const baseClasses = [
    "inline-flex items-center justify-center",
    "font-medium rounded-lg border",
    "transition-all duration-200 ease-in-out",
    "focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500",
    "hover:-translate-y-0.5 active:translate-y-0",
    "select-none",
  ];

  const classes = [
    ...baseClasses,
    variantStyles[variant],
    sizeStyles[size],
    fullWidth ? "w-full" : "",
    disabled ? disabledStyles : "",
    loading ? loadingStyles : "",
    className,
  ]
    .filter(Boolean)
    .join(" ");

  return classes;
}

function ButtonContent({
  leftIcon,
  rightIcon,
  loading,
  children,
}: {
  leftIcon?: React.ReactNode;
  rightIcon?: React.ReactNode;
  loading?: boolean;
  children: React.ReactNode;
}) {
  return (
    <>
      {loading && (
        <svg
          className="w-4 h-4 mr-2 animate-spin"
          fill="none"
          stroke="currentColor"
          viewBox="0 0 24 24"
        >
          <path
            strokeLinecap="round"
            strokeLinejoin="round"
            strokeWidth={2}
            d="M4 4v5h.582m15.356 2A8.001 8.001 0 004.582 9m0 0H9m11 11v-5h-.581m0 0a8.003 8.003 0 01-15.357-2m15.357 2H15"
          />
        </svg>
      )}
      {!loading && leftIcon && <span className="mr-2">{leftIcon}</span>}
      <span>{children}</span>
      {!loading && rightIcon && <span className="ml-2">{rightIcon}</span>}
    </>
  );
}

export function Button(props: ButtonProps) {
  const classes = getButtonClasses(props);
  const { leftIcon, rightIcon, loading, disabled, children } = props;

  if (isLinkButton(props)) {
    const { to, replace } = props;

    return (
      <Link
        to={to}
        replace={replace}
        className={classes}
        style={disabled ? { pointerEvents: "none" } : undefined}
      >
        <ButtonContent
          leftIcon={leftIcon}
          rightIcon={rightIcon}
          loading={loading}
        >
          {children}
        </ButtonContent>
      </Link>
    );
  }

  const { onClick, type = "button" } = props;

  return (
    <button
      type={type}
      onClick={onClick}
      disabled={disabled || loading}
      className={classes}
    >
      <ButtonContent
        leftIcon={leftIcon}
        rightIcon={rightIcon}
        loading={loading}
      >
        {children}
      </ButtonContent>
    </button>
  );
}
