import * as Redux from 'redux';

import { typecheckNever } from '../utils/utils';

/**
 * The point of this demo is to show what happens first:
 *
 * 1. that the * call to `store.dispatch()` returns, or
 * 2. the listeners registered via `store.subscribe` get called and return.
 *
 * The results show that everything is synchronous: the observers are all
 * finished synchronously first, and only then does `.dispatch` return.
 */

type Model = {
  count: number;
  name?: string;
};

type Msg =
  | { type: 'set_name'; name?: string }
  | { type: 'set_count'; count: number }
  | { type: 'incr_count' };

const reducer = (init: Model) => (
  model: Model | undefined,
  msg: Msg,
): Model => {
  if (model === undefined) {
    return init;
  }
  switch (msg.type) {
    case 'set_name':
      return {
        ...model,
        name: msg.name,
      };
    case 'set_count':
      return {
        ...model,
        count: msg.count,
      };
    case 'incr_count':
      return {
        ...model,
        count: model.count + 1,
      };
    default:
      return typecheckNever(msg);
  }
};

const init: Model = {
  count: 0,
};

const store: Redux.Store<Model, Msg> = Redux.createStore<
  Model,
  Msg,
  never,
  never
>(reducer(init));

const listener = () => {
  console.log(`Observed: state.count = ${store.getState().count}`);
};

const unsubscribe = store.subscribe(listener);

const l = (...args: any[]) => console.log(...args); // eslint-disable-line @typescript-eslint/no-explicit-any
const inspect = <T>(v: T) => JSON.stringify(v, undefined, 4);

l('Hi');
l(`Now we dispatch an increment`);
l(`Dispatch returned: ${inspect(store.dispatch({ type: 'incr_count' }))}`);
l(
  `Synchronously immediately after the dispatch call: store.count = ${
    store.getState().count
  }`,
);

l('Goodbye');

unsubscribe();
