import { User } from './twitter/models';
import { typecheckNever } from './utils/utils';
import { RemoteData } from './utils/remote-data';
import * as remoteData from './utils/remote-data';

export type Model = {
  user: RemoteData<User, Error>;
  errors: Error[];
};

export const init: Model = {
  user: { type: 'idle' },
  errors: [],
};

export type Msg =
  | { type: 'fetch_me'; sub: RemoteData<User, Error> }
  | { type: 'add_error'; error: Error }
  | { type: 'remove_error'; error: Error }
  | { type: 'noop' };

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
    case 'remove_error': {
      return {
        ...model,
        errors: model.errors.filter((e) => e !== msg.error),
      };
    }
    case 'noop':
      return model;
    default:
      typecheckNever(msg);
      return model;
  }
};
