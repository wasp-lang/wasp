// TODO: Add messages page
import type { AuthUser } from "wasp/auth";
import { DefaultLayout } from "../../layout/DefaultLayout";

export function MessagesPage({ user }: { user: AuthUser }) {
  return (
    <DefaultLayout user={user}>
      <div>This page is under construction 🚧</div>
    </DefaultLayout>
  );
}
