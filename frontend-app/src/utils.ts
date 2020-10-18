/**
 * Returns any type, instead of returning `never`,
 * in order to not create "unreachable" code.
 */
export function typecheckNever<T>(n: never): T {
  return n;
}

export function pretty(o: unknown): string {
  return JSON.stringify(o, undefined, 4);
}
