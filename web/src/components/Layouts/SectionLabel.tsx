const SectionLabel = ({
  text,
  bgColorClassName = "bg-wasp-yellow",
  textColorClassName = "text-wasp-black",
}: {
  text: string;
  bgColorClassName?: string;
  textColorClassName?: string;
}) => (
  <div
    className={`mb-6 inline-block ${bgColorClassName} px-4 py-1.5 font-mono text-xs font-bold uppercase tracking-widest ${textColorClassName}`}
  >
    // {text}
  </div>
);

export default SectionLabel;
