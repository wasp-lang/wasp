import { type PageViewSource } from "wasp/entities";

export function SourcesTable({
  sources,
}: {
  sources: PageViewSource[] | undefined;
}) {
  return (
    <div className="border-border bg-card shadow-default sm:px-7.5 rounded-sm border px-5 pb-2.5 pt-6 xl:pb-1">
      <h4 className="text-foreground mb-6 text-xl font-semibold">
        Top Sources
      </h4>

      <div className="flex flex-col">
        <div className="bg-gray-2 grid grid-cols-3 rounded-sm">
          <div className="p-2.5 xl:p-5">
            <h5 className="xsm:text-base text-sm font-medium uppercase">
              Source
            </h5>
          </div>
          <div className="p-2.5 text-center xl:p-5">
            <h5 className="xsm:text-base text-sm font-medium uppercase">
              Visitors
            </h5>
          </div>
          <div className="hidden p-2.5 text-center sm:block xl:p-5">
            <h5 className="xsm:text-base text-sm font-medium uppercase">
              Sales
            </h5>
          </div>
        </div>

        {sources && sources.length > 0 ? (
          sources.map((source) => (
            <div
              key={source.name}
              className="border-border grid grid-cols-3 border-b"
            >
              <div className="flex items-center gap-3 p-2.5 xl:p-5">
                <p className="text-foreground">{source.name}</p>
              </div>

              <div className="flex items-center justify-center p-2.5 xl:p-5">
                <p className="text-foreground">{source.visitors}</p>
              </div>

              <div className="hidden items-center justify-center p-2.5 sm:flex xl:p-5">
                <p className="text-foreground">--</p>
              </div>
            </div>
          ))
        ) : (
          <div className="flex items-center justify-center p-2.5 xl:p-5">
            <p className="text-foreground">No data to display</p>
          </div>
        )}
      </div>
    </div>
  );
}
