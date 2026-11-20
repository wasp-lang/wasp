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
          "border-line bg-surface text-ink placeholder:text-gray w-full rounded-md border px-3 py-2.5",
          className,
        )}
        aria-invalid={!!fieldState.error}
        aria-describedby={fieldState.error ? `${id}-error` : undefined}
        {...props}
      />

      {fieldState.error && (
        <span id={`${id}-error`} className="text-error text-sm">
          {fieldState.error.message}
        </span>
      )}
    </div>
  );
}
