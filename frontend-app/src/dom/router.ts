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

export interface IEffects {
  push(path: string): Msg;
  go(n: number): Msg;
}

export class Effects implements IEffects {
  constructor(private readonly hist: history.History<MyLocationState>) {}

  push(url: string): Msg {
    this.hist.push(url);
    return noop();
  }

  go(n: number): Msg {
    this.hist.go(n);
    return noop();
  }
}

export const listener = <T>(dispatch: (_: Msg) => T) => (
  location: history.Location<MyLocationState>,
  _action: string,
): T => {
  return dispatch({ type: 'update_location', location });
};
