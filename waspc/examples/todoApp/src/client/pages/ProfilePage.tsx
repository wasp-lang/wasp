import React from "react";
import { Link } from "react-router-dom";
import { User } from "@wasp/entities";

export const ProfilePage = ({ user: { username } }: { user: User }) => {
    return (
        <>
            <div>I am Profile page for {username}!</div>
            <br />
            <Link to="/">Go to dashboard</Link>
        </>
    );
};
