import { Tag } from "@site/src/components/Tag";

// Used to mark something as related to the server
export function Server() {
  return <Tag color="#0284c7">server</Tag>;
}

// Used to mark something as related to the client
export function Client() {
  return <Tag color="#f97316">client</Tag>;
}

// Used to mark something as related to the database
// e.g. in the deployment guides.
export function Database() {
  return <Tag color="#a855f7">database</Tag>;
}
