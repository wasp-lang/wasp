export function SimplePageContainer({ children }: React.PropsWithChildren<{}>) {
  return (
    <div className="flex justify-center">
      <div className="container px-4 md:px-0 lg:w-1/2">{children}</div>
    </div>
  );
}
