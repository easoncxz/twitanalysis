import React from 'react';
import { typecheckNever } from './utils';

/**
 * Can be used in either a Model or a Msg
 */
export type RemoteData<T, E> =
  | { type: 'idle' }
  | { type: 'loading' }
  | { type: 'ok'; data: T }
  | { type: 'error'; error: E };

export const idle = <T, E = Error>(): RemoteData<T, E> => ({ type: 'idle' });

export const loading = <T, E = Error>(): RemoteData<T, E> => ({
  type: 'loading',
});

export function reduce<T, E = Error>(
  model: RemoteData<T, E>,
  msg: RemoteData<T, E>,
): RemoteData<T, E> {
  switch (msg.type) {
    case 'idle':
      return { type: 'idle' };
    case 'loading':
      return { type: 'loading' };
    case 'ok':
      return { type: 'ok', data: msg.data };
    case 'error':
      return { type: 'error', error: msg.error };
    default:
      typecheckNever(msg);
      return model;
  }
}

export function simpleView<T, E extends Error>(
  model: RemoteData<T, E>,
  ok: (_: T) => React.ReactElement | null,
): React.ReactElement | null {
  switch (model.type) {
    case 'idle':
      return <p>idle</p>;
    case 'loading':
      return <p>loading</p>;
    case 'ok':
      return ok(model.data);
    case 'error':
      return <p>error: {model.error.message}</p>;
  }
}

export function toMaybeDefined<T, E = Error>(
  d: RemoteData<T, E>,
): T | undefined {
  switch (d.type) {
    case 'idle':
    case 'loading':
    case 'error':
      return undefined;
    case 'ok':
      return d.data;
    default:
      typecheckNever(d);
      return undefined;
  }
}

/**
 * Probably not worth using longer-term. TypeScript's type-inference here
 * just isn't getting the job done -- explicit T and E will need to be
 * specified at the call site. Bad!
 */
export const launchPromise = <T, E = Error>(
  /**
   * The return-type of `launchPromise` and the type of this `dispatch`
   * function would require more thought. If this doesn't return void,
   * it's hard to get the call-site to compile.
   */
  dispatch: (_: RemoteData<T, E>) => void,
  action: () => Promise<T>,
): RemoteData<T, E> => {
  action().then(
    (x: T) => dispatch({ type: 'ok', data: x }),
    (e: E) => dispatch({ type: 'error', error: e }),
  );
  return { type: 'loading' };
};

export const map = <A, B, E>(f: (_: A) => B) => (
  d: RemoteData<A, E>,
): RemoteData<B, E> => {
  switch (d.type) {
    case 'idle':
    case 'loading':
    case 'error':
      return d;
    case 'ok':
      return { type: 'ok', data: f(d.data) };
    default:
      typecheckNever(d);
      return d;
  }
};
