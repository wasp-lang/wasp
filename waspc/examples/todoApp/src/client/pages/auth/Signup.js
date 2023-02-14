import { Link } from "react-router-dom";

import SignupForm from "@wasp/auth/forms/Signup";
import getNumTasks from "@wasp/queries/getNumTasks";
import { useQuery } from "@wasp/queries";
import useAuth from "@wasp/auth/useAuth";
import AuthSuccessRedirect from "@wasp/auth/components/AuthSuccessRedirect";

const Signup = () => {
    const { data: user } = useAuth();
    const { data: numTasks } = useQuery(getNumTasks);

    if (user) {
        return <AuthSuccessRedirect />;
    }
    return (
        <>
            <SignupForm />
            <br />
            <span>
                I already have an account (<Link to="/login">go to login</Link>
                ).
            </span>
            <br />
            <span>Number of tasks already created: {numTasks}</span>
        </>
    );
};

export default Signup;
