/**
 * Drop-in section wrapper matching VCCodeComparison's max-w-6xl px-6 layout.
 */
export const VCSection = ({ children, className = "", id }) => {
  return (
    <div
      className={`sm:py-18 mx-auto max-w-6xl px-6 py-16 md:py-24 lg:py-24 ${className}`}
      id={id}
    >
      {children}
    </div>
  );
};

/**
 * Reusable install command code block with copy button.
 */
import { useState } from "react";
import { Check, Copy } from "react-feather";

export const InstallBlock = ({
  command = "npm i -g @wasp.sh/wasp-cli",
  className = "",
}) => {
  const [copied, setCopied] = useState(false);

  const handleCopy = () => {
    navigator.clipboard.writeText(command);
    setCopied(true);
    setTimeout(() => setCopied(false), 1500);
  };

  return (
    <code
      className={`inline-flex items-center gap-2 rounded-none border-0 bg-neutral-100 px-4 py-2 text-sm text-neutral-500 ${className}`}
    >
      <span>
        <span className="text-yellow-400">%</span> {command}
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
