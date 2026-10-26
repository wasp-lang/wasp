import { ComponentProps, useId } from "react";
import { ControllerFieldState } from "react-hook-form";
import { twJoin } from "tailwind-merge";

interface InputProps extends Omit<ComponentProps<"input">, "children" | "id"> {
  label: string;
  fieldState: ControllerFieldState;
}

export function Input({ className, label, fieldState, ...props }: InputProps) {
  const id = useId();

  return (
    <div className="flex flex-col gap-1">
      <label htmlFor={id} className="label">
        {label}
      </label>

      <input
        id={id}
        className={twJoin(
          "w-full border-2 border-black bg-white px-4 py-3 text-black placeholder:text-gray",
          className,
        )}
        {...props}
      />

      {fieldState.error && (
        <span className="text-sm text-red-600">{fieldState.error.message}</span>
      )}
    </div>
  );
}
