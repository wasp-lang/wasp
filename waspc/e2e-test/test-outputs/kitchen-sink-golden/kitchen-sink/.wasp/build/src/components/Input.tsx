import React, { forwardRef } from "react";
import { cn } from "../cn";

interface InputProps
  extends Omit<
    React.InputHTMLAttributes<HTMLInputElement>,
    "id" | "className"
  > {
  label?: string;
  inputClassName?: string;
  containerClassName?: string;
}

export const Input = forwardRef<HTMLInputElement, InputProps>((props, ref) => {
  const {
    type = "text",
    label,
    required,
    disabled,
    containerClassName,
    inputClassName,
    ...textProps
  } = props;

  const inputClasses = cn(
    "block w-full px-3 py-2 rounded-lg border transition-colors duration-200 ease-in-out",
    "focus:outline-none focus:ring-2 focus:ring-offset-0",
    "placeholder-gray-400",
    "disabled:opacity-50 disabled:cursor-not-allowed disabled:bg-gray-50",
    "bg-white border-gray-300 focus:border-primary-500 focus:ring-primary-500",
    inputClassName,
  );

  const inputId = React.useId();

  return (
    <div className={cn("space-y-1", containerClassName)}>
      {label && (
        <label
          htmlFor={inputId}
          className="block text-sm font-medium text-gray-700"
        >
          {label}
          {required && <span className="ml-1 text-red-500">*</span>}
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
