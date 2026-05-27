import { useWindowSize } from "@docusaurus/theme-common";
import TabItem from "@theme/TabItem";
import Tabs from "@theme/Tabs";
import * as React from "react";
import { BeforeAfterProvider, useBeforeAfterContext } from "./Context";

export default function BeforeAfter({
  children,
}: {
  children: React.ReactNode;
}) {
  const windowSize = useWindowSize();
  const sideBySide = windowSize === "desktop";

  const ctxValue = React.useMemo(() => ({ sideBySide }), [sideBySide]);

  return (
    <BeforeAfterProvider value={ctxValue}>
      {sideBySide ? (
        <div className="grid grid-cols-2 gap-4">{children}</div>
      ) : (
        <Tabs
          values={[
            { value: "before", label: "Before" },
            { value: "after", label: "After" },
          ]}
        >
          {children}
        </Tabs>
      )}
    </BeforeAfterProvider>
  );
}

export function Before({ children }: { children?: React.ReactNode }) {
  const { sideBySide } = useBeforeAfterContext();

  return sideBySide ? (
    <Column label="Before">{children}</Column>
  ) : (
    <TabItem value="before" label="Before">
      {children}
    </TabItem>
  );
}

export function After({ children }: { children?: React.ReactNode }) {
  const { sideBySide } = useBeforeAfterContext();

  return sideBySide ? (
    <Column label="After">{children}</Column>
  ) : (
    <TabItem value="after" label="After">
      {children}
    </TabItem>
  );
}

function Column({
  label,
  children,
}: {
  label: string;
  children?: React.ReactNode;
}) {
  return (
    <div>
      <div className="mb-2 text-sm font-semibold uppercase tracking-wider text-wasp-g5">
        {label}
      </div>
      {children}
    </div>
  );
}
