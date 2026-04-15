import { Check } from "react-feather";

const items = [
  "Auth",
  "Database",
  "SaaS Template",
  "Email",
  "Cron Jobs",
  "Deploy",
  "Free & Open-source",
];

const VCIncludedStrip = () => {
  return (
    <div className="mx-auto max-w-4xl px-6 py-8">
      <div className="flex flex-wrap items-center justify-center gap-x-6 gap-y-3">
        {items.map((item) => (
          <span
            key={item}
            className="flex items-center gap-1.5 text-sm text-neutral-500"
          >
            <Check size={14} className="text-yellow-500" strokeWidth={3} />
            {item}
          </span>
        ))}
      </div>
    </div>
  );
};

export default VCIncludedStrip;
