import type { ReactNode } from "react";

// Flat, square-cornered card in the wasp.sh style: a colored header with the
// node kind, then the node name. `children` go under the name (chips, handles).
export function NodeCard({
  kind,
  icon,
  headerClassName,
  name,
  className = "",
  children,
}: {
  kind: string;
  icon: ReactNode;
  headerClassName: string;
  name: ReactNode;
  className?: string;
  children?: ReactNode;
}) {
  return (
    <div
      className={`border-wasp-black bg-wasp-white text-wasp-black border-[1.25px] font-mono ${className}`}
    >
      <div
        className={`border-wasp-black flex items-center gap-1.5 border-b-[1.25px] px-3 py-1 text-[10px] font-bold tracking-widest uppercase ${headerClassName}`}
      >
        {icon}
        <span>{kind}</span>
      </div>
      <div className="flex flex-col items-center gap-2 px-4 py-2.5 text-center">
        <div className="text-sm font-bold">{name}</div>
        {children}
      </div>
    </div>
  );
}

export function Chip({ children }: { children: ReactNode }) {
  return (
    <div className="border-wasp-g4 bg-wasp-bg text-wasp-g7 border px-1.5 py-0.5 text-[11px]">
      {children}
    </div>
  );
}

// Outline icon from Heroicons, `d` is the path data.
export function Icon({
  d,
  className = "h-3 w-3",
  label,
}: {
  d: string;
  className?: string;
  label?: string;
}) {
  return (
    <svg
      xmlns="http://www.w3.org/2000/svg"
      fill="none"
      viewBox="0 0 24 24"
      strokeWidth={2}
      stroke="currentColor"
      className={className}
      role={label ? "img" : undefined}
      aria-label={label}
      aria-hidden={label ? undefined : true}
    >
      {label && <title>{label}</title>}
      <path strokeLinecap="round" strokeLinejoin="round" d={d} />
    </svg>
  );
}
