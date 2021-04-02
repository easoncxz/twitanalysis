import * as history from 'history';

import { typecheckNever } from './utils/utils';

type MyLocationState = unknown;
type Location = history.Location<MyLocationState>;

export type Model = {
  location: Location;
};

export type Msg =
  | { type: 'update_location'; location: Location }
  | { type: 'noop' };

export const noop = (): Msg => ({ type: 'noop' });

export const reduce = (init: Model) => (
  model: Model | undefined,
  msg: Msg,
): Model => {
  if (model === undefined) {
    return init;
  }
  switch (msg.type) {
    case 'noop':
      return model;
    case 'update_location':
      return {
        ...model,
        location: msg.location,
      };
    default:
      return typecheckNever(msg);
  }
};

export type Effects = {
  push(path: string): Msg;
  go(n: number): Msg;
};

export const effectsOf = (hist: history.History<MyLocationState>): Effects => ({
  push(url: string): Msg {
    hist.push(url);
    return noop();
  },
  go(n: number): Msg {
    hist.go(n);
    return noop();
  },
});

export const listener = <T>(dispatch: (_: Msg) => T) => (
  location: history.Location<MyLocationState>,
  _action: string,
): T => {
  return dispatch({ type: 'update_location', location });
};
