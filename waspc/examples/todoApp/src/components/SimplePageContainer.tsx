export function SimplePageContainer({ children }: React.PropsWithChildren<{}>) {
  return (
    <div className="flex justify-center">
      <div className="w-1/2">{children}</div>
    </div>
  );
}
