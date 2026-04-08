import { cn } from "../cn";

type AlertVariant = "info" | "success" | "warning" | "error";

interface AlertProps {
  variant?: AlertVariant;
  className?: string;
}

const variantStyles: Record<AlertVariant, string> = {
  info: "bg-blue-50 border-blue-200 text-blue-700",
  success: "bg-green-50 border-green-200 text-green-700",
  warning: "bg-yellow-50 border-yellow-200 text-yellow-700",
  error: "bg-red-50 border-red-200 text-red-700",
};

export function Alert({
  children,
  className,
  variant = "info",
}: React.PropsWithChildren<AlertProps>) {
  return (
    <div
      className={cn(
        "rounded-lg border px-4 py-3",
        "text-sm font-medium",
        variantStyles[variant],
        className,
      )}
    >
      {children}
    </div>
  );
}
