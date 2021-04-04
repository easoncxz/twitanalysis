import { User } from './twitter/models';
import { typecheckNever } from './utils/utils';
import { RemoteData } from './utils/remote-data';
import * as remoteData from './utils/remote-data';
import * as fetchMe from './pages/fetch-me';

export type Model = {
  user: RemoteData<User, Error>;
  errors: Error[];
};

export type Msg =
  | { type: 'fetch_me'; sub: fetchMe.Msg }
  | { type: 'add_error'; error: Error }
  | { type: 'clear_error'; error: Error }
  | { type: 'noop' };

export const noop = (): Msg => ({ type: 'noop' });

export const init: Model = {
  user: { type: 'idle' },
  errors: [],
};

export const reduce = (init: Model) => (
  model: Model | undefined,
  msg: Msg,
): Model => {
  if (!model) {
    return init;
  }
  switch (msg.type) {
    case 'fetch_me':
      return {
        ...model,
        user: remoteData.reduce(model.user, msg.sub),
      };
    case 'add_error': {
      return {
        ...model,
        errors: model.errors.concat([msg.error]),
      };
    }
    case 'clear_error': {
      return {
        ...model,
        errors: model.errors.filter((e) => e !== msg.error),
      };
    }
    case 'noop':
      return model;
    default:
      // Can't just use an ordinary `(n: never) => never` function,
      // because Redux actually abuse our reducer function to run
      // their internal actions. We must return the model despite
      // semantically it's more sensible to throw an error.
      typecheckNever(msg);
      return model;
  }
};
