import { ComponentProps } from "react";
import { ClassNameValue, twJoin } from "tailwind-merge";
import { Link } from "wasp/client/router";

type ButtonSize = "md" | "sm";
type ButtonVariant = "primary" | "ghost";

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
    "inline-flex cursor-pointer items-center justify-center gap-2 border-2 font-bold disabled:cursor-not-allowed disabled:opacity-50",
    variantStyles[variant],
    sizeStyles[size],
    className,
  );
}

const sizeStyles: Record<ButtonSize, ClassNameValue> = {
  md: "px-5 py-3",
  sm: "px-4 py-2",
};

const variantStyles: Record<ButtonVariant, ClassNameValue> = {
  primary: "border-black bg-yellow text-black hover:bg-yellow-dark",
  ghost: "border-transparent bg-transparent text-black hover:border-black",
};
