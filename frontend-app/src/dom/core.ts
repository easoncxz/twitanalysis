import { User, Status } from './twitter/models';
import { typecheckNever } from './utils/utils';
import { RemoteData } from './utils/remote-data';
import * as remoteData from './utils/remote-data';
import * as fetchMe from './pages/fetch-me';

export type Model = {
  // Data
  user: RemoteData<User, Error>;
  sentTweets: Status[];

  // UI
  pendingTweet: string;
  errors: Error[];

  // Network state
  fetchingFaves: boolean;
  sendingTweet: boolean;
};

export type Msg =
  | { type: 'fetch_me'; sub: fetchMe.Msg }
  // sendTweet
  | { type: 'start_send_tweet' }
  | { type: 'receive_send_tweet'; status: Status }
  | { type: 'error_send_tweet'; error: Error }
  // text fields
  | { type: 'update_pending_tweet'; text: string }
  // misc
  | { type: 'add_error'; error: Error }
  | { type: 'clear_error'; error: Error }
  | { type: 'noop' };

export const noop = (): Msg => ({ type: 'noop' });

export const init: Model = {
  user: { type: 'idle' },
  sentTweets: [],
  pendingTweet: '(initial)',
  errors: [],
  fetchingFaves: false,
  sendingTweet: false,
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
    case 'start_send_tweet':
      return {
        ...model,
        sendingTweet: true,
      };
    case 'receive_send_tweet': {
      const sentTweets = model.sentTweets.slice();
      sentTweets.push(msg.status);
      return {
        ...model,
        sentTweets,
        sendingTweet: false,
      };
    }
    case 'error_send_tweet': {
      return {
        ...model,
        sendingTweet: false,
        errors: model.errors.concat([msg.error]),
      };
    }
    case 'update_pending_tweet':
      return {
        ...model,
        pendingTweet: msg.text,
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
