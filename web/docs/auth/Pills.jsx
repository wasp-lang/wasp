import React from "react";

export function Pill({ children, linkToPage, style = {} }) {
  return <a href={linkToPage}
    style={{
      padding: "0.2rem 0.5rem",
      borderRadius: "0.375rem",
      color: "#333",
      textDecoration: "none",
      display: "inline-block",
      ...style,
    }}
  >{children}</a>;
}

export function EmailPill() {
  return <Pill style={{
    backgroundColor: "#e0f2fe ",
  }} linkToPage="/docs/auth/email">Email</Pill>;
}

export function UsernameAndPasswordPill() {
  return <Pill style={{
    backgroundColor: "#fce7f3",
  }} linkToPage="/docs/auth/username-and-pass">Username & Password</Pill>;
}

export function GithubPill() {
  return <Pill style={{
    backgroundColor: "#f1f5f9",
  }} linkToPage="/docs/auth/github">Github</Pill>;
}

export function GooglePill() {
  return <Pill style={{
    backgroundColor: "#ecfccb",
  }} linkToPage="/docs/auth/google">Google</Pill>;
}
