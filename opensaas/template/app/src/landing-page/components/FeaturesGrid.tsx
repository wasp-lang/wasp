import React from "react";
import {
  Card,
  CardContent,
  CardDescription,
  CardTitle,
} from "../../client/components/ui/card";
import { cn } from "../../client/utils";
import { Feature } from "./Features";
import { SectionTitle } from "./SectionTitle";

export interface GridFeature extends Omit<Feature, "icon"> {
  icon?: React.ReactNode;
  emoji?: string;
  direction?: "col" | "row" | "col-reverse" | "row-reverse";
  align?: "center" | "left";
  size: "small" | "medium" | "large";
  fullWidthIcon?: boolean;
}

interface FeaturesGridProps {
  features: GridFeature[];
  className?: string;
}

export function FeaturesGrid({ features, className = "" }: FeaturesGridProps) {
  return (
    <div
      className="mx-auto my-16 flex max-w-7xl flex-col gap-4 md:my-24 lg:my-40"
      id="features"
    >
      <SectionTitle
        title="Features"
        description="These are some of the features of the product."
      />
      <div
        className={cn(
          "mx-4 grid auto-rows-[minmax(140px,auto)] grid-cols-2 gap-4 md:mx-6 md:grid-cols-4 lg:mx-8 lg:grid-cols-6",
          className,
        )}
      >
        {features.map((feature) => (
          <FeaturesGridItem
            key={feature.name + feature.description}
            {...feature}
          />
        ))}
      </div>
    </div>
  );
}

function FeaturesGridItem({
  name,
  description,
  icon,
  emoji,
  href,
  direction = "col",
  align = "center",
  size = "medium",
  fullWidthIcon = true,
}: GridFeature) {
  const gridFeatureSizeToClasses: Record<GridFeature["size"], string> = {
    small: "col-span-1",
    medium: "col-span-2 md:col-span-2 lg:col-span-2",
    large: "col-span-2 md:col-span-2 lg:col-span-2 row-span-2",
  };

  const directionToClass: Record<
    NonNullable<GridFeature["direction"]>,
    string
  > = {
    col: "flex-col",
    row: "flex-row",
    "row-reverse": "flex-row-reverse",
    "col-reverse": "flex-col-reverse",
  };

  const gridFeatureCard = (
    <Card
      className={cn(
        "h-full min-h-[140px] cursor-pointer transition-all duration-300 hover:shadow-lg",
        gridFeatureSizeToClasses[size],
      )}
      variant="bento"
    >
      <CardContent className="flex h-full flex-col items-center justify-center p-4">
        {fullWidthIcon && (icon || emoji) ? (
          <div className="mb-3 flex w-full items-center justify-center">
            {icon ? (
              icon
            ) : emoji ? (
              <span className="text-4xl">{emoji}</span>
            ) : null}
          </div>
        ) : (
          <div
            className={cn(
              "flex items-center gap-3",
              directionToClass[direction],
              align === "center"
                ? "items-center justify-center"
                : "justify-start",
            )}
          >
            <div className="flex h-10 w-10 items-center justify-center rounded-lg">
              {icon ? (
                icon
              ) : emoji ? (
                <span className="text-2xl">{emoji}</span>
              ) : null}
            </div>
            <CardTitle
              className={cn(align === "center" ? "text-center" : "text-left")}
            >
              {name}
            </CardTitle>
          </div>
        )}
        {fullWidthIcon && (icon || emoji) && (
          <CardTitle className="mb-2 text-center">{name}</CardTitle>
        )}
        <CardDescription
          className={cn(
            "text-xs leading-relaxed",
            fullWidthIcon || direction === "col" || align === "center"
              ? "text-center"
              : "text-left",
          )}
        >
          {description}
        </CardDescription>
      </CardContent>
    </Card>
  );

  if (href) {
    return (
      <a
        href={href}
        target="_blank"
        rel="noopener noreferrer"
        className={gridFeatureSizeToClasses[size]}
      >
        {gridFeatureCard}
      </a>
    );
  }

  return gridFeatureCard;
}
