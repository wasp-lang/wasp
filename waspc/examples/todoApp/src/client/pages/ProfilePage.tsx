import React from "react";
import { Link } from "react-router-dom";
import { User } from "@wasp/entities";

export const ProfilePage = ({ user: { username } }: { user: User }) => {
    return (
        <>
            <h2>Profile page</h2>
            <div>
                Hello <strong>{username}</strong>!
            </div>
            <br />
            <Link to="/">Go to dashboard</Link>
        </>
    );
};
