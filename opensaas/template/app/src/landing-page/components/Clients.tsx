import { AstroLogo } from "../logos/AstroLogo";
import { OpenAILogo } from "../logos/OpenAILogo";
import { PrismaLogo } from "../logos/PrismaLogo";
import { SalesforceLogo } from "../logos/SalesforceLogo";

const LOGOS = [
  { id: "salesforce", element: <SalesforceLogo /> },
  { id: "prisma", element: <PrismaLogo /> },
  { id: "astro", element: <AstroLogo /> },
  { id: "openai", element: <OpenAILogo /> },
];

export function Clients() {
  return (
    <div className="items-between mx-auto mt-12 flex max-w-7xl flex-col gap-y-6 px-6 lg:px-8">
      <h2 className="text-muted-foreground mb-6 text-center font-semibold tracking-wide">
        Built with / Used by:
      </h2>

      <div className="mx-auto grid max-w-lg grid-cols-2 items-center gap-x-8 gap-y-12 sm:max-w-xl sm:gap-x-10 sm:gap-y-14 md:grid-cols-4 lg:mx-0 lg:max-w-none">
        {LOGOS.map((logo) => (
          <div
            key={logo.id}
            className="col-span-1 flex max-h-12 w-full justify-center object-contain opacity-80 transition-opacity hover:opacity-100"
          >
            {logo.element}
          </div>
        ))}
      </div>
    </div>
  );
}
