import React, { forwardRef } from "react";

type InputVariant = "default" | "filled" | "minimal";

type InputSize = "sm" | "md" | "lg";

type InputState = "default" | "error" | "success" | "warning";

interface BaseInputProps {
  label?: string;
  helperText?: string;
  errorText?: string;
  successText?: string;
  warningText?: string;
  variant?: InputVariant;
  size?: InputSize;
  state?: InputState;
  required?: boolean;
  disabled?: boolean;
  inputClassName?: string;
  containerClassName?: string;
}

interface InputProps extends BaseInputProps {
  type?: "text" | "email" | "password" | "url" | "tel" | "search";
  placeholder?: string;
  value?: string;
  defaultValue?: string;
  onChange?: (e: React.ChangeEvent<HTMLInputElement>) => void;
  onFocus?: (e: React.FocusEvent<HTMLInputElement>) => void;
  onBlur?: (e: React.FocusEvent<HTMLInputElement>) => void;
  maxLength?: number;
  minLength?: number;
  pattern?: string;
  autoComplete?: string;
  autoFocus?: boolean;
}

const variantStyles: Record<InputVariant, string> = {
  default:
    "bg-white border-gray-300 focus:border-indigo-500 focus:ring-indigo-500",
  filled:
    "bg-gray-50 border-gray-200 focus:bg-white focus:border-indigo-500 focus:ring-indigo-500",
  minimal:
    "bg-transparent border-0 border-b-2 border-gray-300 focus:border-indigo-500 rounded-none focus:ring-0 px-0",
};

const sizeStyles: Record<InputSize, string> = {
  sm: "px-3 py-1.5 text-sm",
  md: "px-4 py-2 text-sm",
  lg: "px-4 py-3 text-base",
};

const stateStyles: Record<InputState, string> = {
  default: "",
  error: "border-red-300 focus:border-red-500 focus:ring-red-500",
  success: "border-green-300 focus:border-green-500 focus:ring-green-500",
  warning: "border-amber-300 focus:border-amber-500 focus:ring-amber-500",
};

function getInputClasses(props: InputProps): string {
  const {
    variant = "default",
    size = "md",
    state = "default",
    disabled = false,

    inputClassName = "",
  } = props;

  const baseClasses = [
    "block rounded-lg border transition-colors duration-200 ease-in-out",
    "focus:outline-none focus:ring-2 focus:ring-offset-0",
    "placeholder-gray-400",
    "disabled:opacity-50 disabled:cursor-not-allowed disabled:bg-gray-50",
  ];

  const classes = [
    ...baseClasses,
    variantStyles[variant],
    sizeStyles[size],
    stateStyles[state],
    inputClassName,
  ]
    .filter(Boolean)
    .join(" ");

  return classes;
}

export const Input = forwardRef<
  HTMLInputElement | HTMLTextAreaElement,
  InputProps
>((props, ref) => {
  const { label, required, disabled, containerClassName = "" } = props;

  const inputClasses = getInputClasses(props);

  const inputId = React.useId();

  const { type = "text", size, ...textProps } = props;

  return (
    <div className={`space-y-1 ${containerClassName}`}>
      {label && (
        <label
          htmlFor={inputId}
          className="block text-sm font-medium text-gray-700"
        >
          {label}
          {required && <span className="text-red-500 ml-1">*</span>}
        </label>
      )}

      <input
        ref={ref as React.Ref<HTMLInputElement>}
        id={inputId}
        type={type}
        disabled={disabled}
        required={required}
        className={inputClasses}
        {...textProps}
      />
    </div>
  );
});
