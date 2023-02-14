{{={= =}=}}
import { Redirect } from 'react-router-dom';

export default function RedirectAfterLoginPage() {
    return <Redirect to="{= onAuthSucceededRedirectTo =}" />;
}
