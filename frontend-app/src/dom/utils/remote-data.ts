/**
 * Can be used in either a Model or a Msg
 */
export type RemoteData<T, E> =
  | { type: 'idle' }
  | { type: 'loading' }
  | { type: 'ok'; data: T }
  | { type: 'error'; error: E };
