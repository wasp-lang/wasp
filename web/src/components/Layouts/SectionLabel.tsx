const variants = {
  yellow: "bg-wasp-yellow text-wasp-black",
  purple: "bg-wasp-purple text-wasp-white",
} as const;

const SectionLabel = ({
  text,
  variant = "yellow",
}: {
  text: string;
  variant?: keyof typeof variants;
}) => (
  <div
    className={`mb-6 inline-block px-4 py-1.5 font-mono text-xs font-bold uppercase tracking-widest ${variants[variant]}`}
  >
    // {text}
  </div>
);

export default SectionLabel;
