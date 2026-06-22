import { SectionTitle } from "./SectionTitle";

export interface Feature {
  name: string;
  description: string;
  icon: string;
  href: string;
}

export function Features({ features }: { features: Feature[] }) {
  return (
    <div id="features" className="mx-auto mt-48 max-w-7xl px-6 lg:px-8">
      <SectionTitle
        title={
          <p className="text-foreground mt-2 text-4xl font-bold tracking-tight sm:text-5xl">
            The <span className="text-secondary">Best</span> Features
          </p>
        }
        description="Don't work harder. Work smarter."
      />
      <div className="mx-auto mt-16 max-w-2xl sm:mt-20 lg:mt-24 lg:max-w-4xl">
        <dl className="grid max-w-xl grid-cols-1 gap-x-8 gap-y-10 lg:max-w-none lg:grid-cols-2 lg:gap-y-16">
          {features.map((feature) => (
            <div key={feature.name} className="relative pl-16">
              <dt className="text-foreground text-base font-semibold leading-7">
                <div className="border-accent bg-accent/30 absolute left-0 top-0 flex h-10 w-10 items-center justify-center rounded-lg border">
                  <div className="text-2xl">{feature.icon}</div>
                </div>
                {feature.name}
              </dt>
              <dd className="text-muted-foreground mt-2 text-base leading-7">
                {feature.description}
              </dd>
            </div>
          ))}
        </dl>
      </div>
    </div>
  );
}
