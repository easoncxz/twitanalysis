import * as history from 'history';
import type * as Redux from 'redux';

import { typecheckNever } from './utils';

type MyLocationState = unknown;
type Location = history.Location<MyLocationState>;

export type Model = {
  location: Location;
};

export type Msg =
  | { type: 'update_location'; location: Location }
  | { type: 'noop' };

const noop = (): Msg => ({ type: 'noop' });

export const reduce = (init: history.Location<MyLocationState>) => (
  model: Model | undefined,
  msg: Msg,
): Model => {
  if (model === undefined) {
    return { location: init };
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
  go(_n: number): Msg {
    console.log("We don't have `history.go` implemented yet");
    return noop();
  },
});

export const listener = (dispatch: Redux.Dispatch<Msg>) => (
  location: history.Location<MyLocationState>,
  action: string,
): Msg => {
  return dispatch({ type: 'update_location', location, action });
};
