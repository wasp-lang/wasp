import { ComponentProps } from "react";
import { ClassNameValue, twJoin } from "tailwind-merge";
import { Link } from "wasp/client/router";

type ButtonSize = "md" | "sm" | "xs";
type ButtonVariant = "primary" | "danger" | "secondary";

interface ButtonProps extends ComponentProps<"button"> {
  size?: ButtonSize;
  variant?: ButtonVariant;
}

export function Button({
  children,
  className,
  type = "button",
  size = "md",
  variant = "primary",
  ...props
}: ButtonProps) {
  return (
    <button
      type={type}
      className={getButtonClasses({
        size,
        variant,
        className,
      })}
      {...props}
    >
      {children}
    </button>
  );
}

type ButtonLinkProps = ComponentProps<typeof Link> & {
  size?: ButtonSize;
  variant?: ButtonVariant;
};

export function ButtonLink({
  children,
  className,
  size = "md",
  variant = "primary",
  ...props
}: ButtonLinkProps) {
  return (
    <Link
      className={getButtonClasses({
        size,
        variant,
        className,
      })}
      {...props}
    >
      {children}
    </Link>
  );
}

function getButtonClasses({
  size,
  variant,
  className,
}: {
  size: ButtonSize;
  variant: ButtonVariant;
  className: ClassNameValue;
}): string {
  return twJoin(
    "border-2 border-neutral-900 font-mono font-bold text-neutral-900 transition-colors",
    variantStyles[variant],
    sizeStyles[size],
    className,
  );
}

const sizeStyles: Record<ButtonSize, ClassNameValue> = {
  md: "px-4 py-2 text-sm",
  sm: "px-3 py-1.5 text-xs",
  xs: "px-2 py-1 text-xs",
};

const variantStyles: Record<ButtonVariant, ClassNameValue> = {
  primary: "bg-primary-500 hover:bg-primary-400 active:bg-primary-300",
  danger: "bg-red-600 text-white hover:bg-red-700 active:bg-red-800",
  secondary: "bg-white hover:bg-primary-100 active:bg-primary-200",
};
