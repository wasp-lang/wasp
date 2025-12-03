export function sayHi() {
    console.log("This is coming from the shared function.")
}


export type NonEmptyArray<T> = [T, ...T[]]
