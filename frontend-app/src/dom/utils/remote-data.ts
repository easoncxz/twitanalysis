import { typecheckNever } from './utils';

/**
 * Can be used in either a Model or a Msg
 */
export type RemoteData<T, E> =
  | { type: 'idle' }
  | { type: 'loading' }
  | { type: 'ok'; data: T }
  | { type: 'error'; error: E };

export function reduce<T, E>(
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
