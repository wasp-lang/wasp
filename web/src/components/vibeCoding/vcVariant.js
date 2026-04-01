/**
 * Resolves Tailwind class strings based on the active variant.
 * When variant is undefined (original page), returns `base`.
 *
 * Usage:
 *   vc(variant, { base: "rounded-lg shadow-sm", v1: "rounded-none", v2: "rounded-none", v3: "rounded-none" })
 */
export function vc(variant, classes) {
  if (!variant) return classes.base;
  return classes[variant] ?? classes.base;
}

/**
 * Drop-in replacement for SectionContainer on variant pages.
 * Matches VCCodeComparison's max-w-6xl px-6 layout.
 * When no variant is set, falls back to the real SectionContainer.
 */
import SectionContainerOrig from "../Layouts/SectionContainer";

export const VCSection = ({ variant, children, className = "", id }) => {
  if (!variant) {
    return <SectionContainerOrig className={className} id={id}>{children}</SectionContainerOrig>;
  }
  return (
    <div
      className={`mx-auto max-w-6xl px-6 py-16 sm:py-18 md:py-24 lg:py-24 ${className}`}
      id={id}
    >
      {children}
    </div>
  );
};

/**
 * Reusable install command code block with copy button, styled per variant.
 */
import { useState } from "react";
import { Copy, Check } from "react-feather";

export const InstallBlock = ({ variant, command = "npm i -g @wasp.sh/wasp-cli", className = "" }) => {
  const [copied, setCopied] = useState(false);

  const handleCopy = () => {
    navigator.clipboard.writeText(command);
    setCopied(true);
    setTimeout(() => setCopied(false), 1500);
  };

  return (
    <code
      className={`${vc(variant, {
        base: "rounded border-0 bg-neutral-100 px-4 py-2 text-sm text-neutral-500",
        v1: "rounded-none border-0 bg-neutral-100 px-4 py-2 text-sm text-neutral-500",
        v2: "rounded-none border-0 bg-neutral-100 px-4 py-2 text-sm text-neutral-500",
        v3: "rounded-none border-0 bg-neutral-100 text-neutral-500 px-4 py-2 text-sm",
      })} inline-flex items-center gap-2 ${className}`}
    >
      <span>
        <span className={variant === "v3" ? "text-yellow-400" : "text-yellow-500"}>%</span> {command}
      </span>
      <button
        onClick={handleCopy}
        className="cursor-pointer text-neutral-400 transition-colors hover:text-neutral-600"
        aria-label="Copy to clipboard"
      >
        {copied ? <Check size={14} /> : <Copy size={14} />}
      </button>
    </code>
  );
};

/**
 * Crosshair corner markers for V2+ cards.
 */
const Crosshair = ({ className }) => (
  <span
    className={`absolute flex h-3 w-3 items-center justify-center select-none text-xs leading-none text-neutral-300 ${className}`}
  >
    +
  </span>
);

export const CrosshairCard = ({ children, className = "" }) => (
  <div className={`relative ${className}`}>
    <Crosshair className="-left-1.5 -top-1.5" />
    <Crosshair className="-right-1.5 -top-1.5" />
    <Crosshair className="-bottom-1.5 -left-1.5" />
    <Crosshair className="-bottom-1.5 -right-1.5" />
    {children}
  </div>
);
