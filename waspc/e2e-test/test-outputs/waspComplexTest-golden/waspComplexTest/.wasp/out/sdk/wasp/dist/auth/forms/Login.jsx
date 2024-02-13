import Auth from './Auth';
import { State } from './types';
// PUBLIC API
export function LoginForm({ appearance, logo, socialLayout, }) {
    return (<Auth appearance={appearance} logo={logo} socialLayout={socialLayout} state={State.Login}/>);
}
//# sourceMappingURL=Login.jsx.map