import Auth from './Auth';
import { State, } from './types';
// PUBLIC API
export function SignupForm({ appearance, logo, socialLayout, additionalFields, }) {
    return (<Auth appearance={appearance} logo={logo} socialLayout={socialLayout} state={State.Signup} additionalSignupFields={additionalFields}/>);
}
//# sourceMappingURL=Signup.jsx.map