export function defineProvider<
  OAuthClient,
  const Id extends string
>({
  id,
  displayName,
  oAuthClient,
}: {
  id: Id;
  displayName: string;
  oAuthClient: OAuthClient;
}) {
  return {
    id,
    displayName,
    oAuthClient,
  };
}
