import * as history from 'history';

import { typecheckNever } from './utils/utils';

type MyDispatch<T> = (_: T) => T;
export type MyLocationState = never;
export type Location = history.Location<MyLocationState>;

export type Model = {
  location: Location;
};

export const init = (location: Location): Model => {
  return { location };
};

export type Msg =
  | { type: 'update_location'; location: Location }
  | { type: 'noop' };

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

export const noop = (): Msg => ({ type: 'noop' });

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

export const listener = (dispatch: MyDispatch<Msg>) => (
  location: history.Location<MyLocationState>,
  _action: string,
): Msg => {
  return dispatch({ type: 'update_location', location });
};
