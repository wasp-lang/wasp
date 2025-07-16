import React from "react";
import { ControllerFieldState } from "react-hook-form";
import { twJoin } from "tailwind-merge";

interface InputProps
  extends Omit<React.InputHTMLAttributes<HTMLInputElement>, "children" | "id"> {
  label: string;
  fieldState: ControllerFieldState;
}

export const Input = React.forwardRef<HTMLInputElement, InputProps>(
  function Input({ className, label, fieldState, ...props }, ref) {
    const id = React.useId();
    return (
      <div className="flex flex-col gap-1">
        <label htmlFor={id} className="label">
          {label}
        </label>
        <input
          id={id}
          className={twJoin(
            "w-full rounded-md border border-neutral-300 bg-white px-3 py-2 text-neutral-800 shadow-sm focus:border-primary-500 focus:outline-none focus:ring-1 focus:ring-primary-500",
            className,
          )}
          {...props}
          ref={ref}
        />
        {fieldState.error && (
          <span className="text-sm text-red-500">
            {fieldState.error.message}
          </span>
        )}
      </div>
    );
  },
);
