/**
 * We deliberately don't use the usual `type="email"` here because that one
 * validates against HTML5 grammar which is stricter than our email validation
 * rules (we additionally allow unicode characters).
 *
 * We relax the `type` to `"text"`, but then set `inputMode` and
 * `autoComplete` to `"email"` to compensate and keep the right experience
 * (e.g. on mobile).
 *
 * @see {@link https://github.com/whatwg/html/issues/4562 WHATWG international email addresses issue}
 */
export declare const emailInputProps: {
    readonly type: "text";
    readonly inputMode: "email";
    readonly autoComplete: "email";
};
export declare const emailFieldRules: {
    required: string;
    setValueAs: (email: string) => string;
    validate: (email: string) => true | "Email must be a valid email";
};
