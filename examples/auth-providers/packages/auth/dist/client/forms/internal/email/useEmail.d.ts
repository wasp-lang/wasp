export declare function useEmail({ onError, showEmailVerificationPending, onLoginSuccess, isLogin, }: {
    onError: (error: Error) => void;
    showEmailVerificationPending: () => void;
    onLoginSuccess: () => void;
    isLogin: boolean;
}): {
    handleSubmit: (data: {
        email: string;
        password: string;
    }) => Promise<void>;
};
