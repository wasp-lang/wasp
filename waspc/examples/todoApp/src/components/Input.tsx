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
  leftIcon?: React.ReactNode;
  rightIcon?: React.ReactNode;
  leftAddon?: React.ReactNode;
  rightAddon?: React.ReactNode;
  className?: string;
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
    leftIcon,
    rightIcon,
    leftAddon,
    rightAddon,
    className = "",
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
    leftIcon || leftAddon ? "pl-10" : "",
    rightIcon || rightAddon ? "pr-10" : "",
    className,
  ]
    .filter(Boolean)
    .join(" ");

  return classes;
}

function getStateMessage(
  props: InputProps,
): { message: string; type: InputState } | null {
  const {
    state = "default",
    errorText,
    successText,
    warningText,
    helperText,
  } = props;

  if (state === "error" && errorText) {
    return { message: errorText, type: "error" };
  }
  if (state === "success" && successText) {
    return { message: successText, type: "success" };
  }
  if (state === "warning" && warningText) {
    return { message: warningText, type: "warning" };
  }
  if (helperText) {
    return { message: helperText, type: "default" };
  }

  return null;
}

function StateMessage({
  message,
  type,
}: {
  message: string;
  type: InputState;
}) {
  const iconMap = {
    error: (
      <svg
        className="w-4 h-4"
        fill="none"
        stroke="currentColor"
        viewBox="0 0 24 24"
      >
        <path
          strokeLinecap="round"
          strokeLinejoin="round"
          strokeWidth={2}
          d="M12 8v4m0 4h.01M21 12a9 9 0 11-18 0 9 9 0 0118 0z"
        />
      </svg>
    ),
    success: (
      <svg
        className="w-4 h-4"
        fill="none"
        stroke="currentColor"
        viewBox="0 0 24 24"
      >
        <path
          strokeLinecap="round"
          strokeLinejoin="round"
          strokeWidth={2}
          d="M9 12l2 2 4-4m6 2a9 9 0 11-18 0 9 9 0 0118 0z"
        />
      </svg>
    ),
    warning: (
      <svg
        className="w-4 h-4"
        fill="none"
        stroke="currentColor"
        viewBox="0 0 24 24"
      >
        <path
          strokeLinecap="round"
          strokeLinejoin="round"
          strokeWidth={2}
          d="M12 9v2m0 4h.01m-6.938 4h13.856c1.54 0 2.502-1.667 1.732-2.5L13.732 4c-.77-.833-1.964-.833-2.732 0L3.732 16.5c-.77.833.192 2.5 1.732 2.5z"
        />
      </svg>
    ),
    default: null,
  };

  const colorMap = {
    error: "text-red-600",
    success: "text-green-600",
    warning: "text-amber-600",
    default: "text-gray-600",
  };

  return (
    <div
      className={`flex items-center space-x-1 text-sm mt-1 ${colorMap[type]}`}
    >
      {iconMap[type]}
      <span>{message}</span>
    </div>
  );
}

export const Input = forwardRef<
  HTMLInputElement | HTMLTextAreaElement,
  InputProps
>((props, ref) => {
  const {
    label,
    required,
    disabled,
    leftIcon,
    rightIcon,
    leftAddon,
    rightAddon,
    containerClassName = "",
  } = props;

  const inputClasses = getInputClasses(props);
  const stateMessage = getStateMessage(props);

  const inputId = React.useId();

  const renderInput = () => {
    const { type = "text", size, ...textProps } = props;
    return (
      <input
        ref={ref as React.Ref<HTMLInputElement>}
        id={inputId}
        type={type}
        disabled={disabled}
        required={required}
        className={inputClasses}
        {...textProps}
      />
    );
  };

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

      <div className="relative">
        {leftAddon && (
          <div className="absolute inset-y-0 left-0 flex items-center">
            <div className="bg-gray-50 border border-r-0 border-gray-300 rounded-l-lg px-3 py-2 text-gray-500 text-sm">
              {leftAddon}
            </div>
          </div>
        )}

        {leftIcon && !leftAddon && (
          <div className="absolute inset-y-0 left-0 flex items-center pl-3 pointer-events-none">
            <div className="text-gray-400">{leftIcon}</div>
          </div>
        )}

        {renderInput()}

        {rightIcon && !rightAddon && (
          <div className="absolute inset-y-0 right-0 flex items-center pr-3 pointer-events-none">
            <div className="text-gray-400">{rightIcon}</div>
          </div>
        )}

        {rightAddon && (
          <div className="absolute inset-y-0 right-0 flex items-center">
            <div className="bg-gray-50 border border-l-0 border-gray-300 rounded-r-lg px-3 py-2 text-gray-500 text-sm">
              {rightAddon}
            </div>
          </div>
        )}
      </div>

      {stateMessage && (
        <StateMessage message={stateMessage.message} type={stateMessage.type} />
      )}
    </div>
  );
});

Input.displayName = "Input";

export function TextInput(
  props: Omit<InputProps, "type"> & {
    type?: "text" | "email" | "password" | "url" | "tel" | "search";
  },
) {
  return <Input {...props} />;
}

export function EmailInput(props: Omit<InputProps, "type">) {
  return <Input {...props} type="email" />;
}

export function PasswordInput(props: Omit<InputProps, "type">) {
  const [showPassword, setShowPassword] = React.useState(false);

  return (
    <Input
      {...props}
      type={showPassword ? "text" : "password"}
      rightIcon={
        <button
          type="button"
          onClick={() => setShowPassword(!showPassword)}
          className="text-gray-400 hover:text-gray-600 focus:outline-none"
        >
          {showPassword ? (
            <svg
              className="w-5 h-5"
              fill="none"
              stroke="currentColor"
              viewBox="0 0 24 24"
            >
              <path
                strokeLinecap="round"
                strokeLinejoin="round"
                strokeWidth={2}
                d="M13.875 18.825A10.05 10.05 0 0112 19c-4.478 0-8.268-2.943-9.543-7a9.97 9.97 0 011.563-3.029m5.858.908a3 3 0 114.243 4.243M9.878 9.878l4.242 4.242M9.878 9.878L8.464 8.464M14.12 14.12l1.415 1.415M14.12 14.12L18.364 18.364M8.464 8.464L6.05 6.05m2.414 2.414L12 12m6.364-6.364L16.95 4.636m1.414 1.414L12 12"
              />
            </svg>
          ) : (
            <svg
              className="w-5 h-5"
              fill="none"
              stroke="currentColor"
              viewBox="0 0 24 24"
            >
              <path
                strokeLinecap="round"
                strokeLinejoin="round"
                strokeWidth={2}
                d="M15 12a3 3 0 11-6 0 3 3 0 016 0z"
              />
              <path
                strokeLinecap="round"
                strokeLinejoin="round"
                strokeWidth={2}
                d="M2.458 12C3.732 7.943 7.523 5 12 5c4.478 0 8.268 2.943 9.542 7-1.274 4.057-5.064 7-9.542 7-4.477 0-8.268-2.943-9.542-7z"
              />
            </svg>
          )}
        </button>
      }
    />
  );
}

export function SearchInput(props: Omit<InputProps, "type" | "leftIcon">) {
  return (
    <Input
      {...props}
      type="search"
      leftIcon={
        <svg
          className="w-5 h-5"
          fill="none"
          stroke="currentColor"
          viewBox="0 0 24 24"
        >
          <path
            strokeLinecap="round"
            strokeLinejoin="round"
            strokeWidth={2}
            d="M21 21l-6-6m2-5a7 7 0 11-14 0 7 7 0 0114 0z"
          />
        </svg>
      }
    />
  );
}
