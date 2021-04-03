import { User, Status } from './twitter/models';
import { typecheckNever } from './utils/utils';
import { RemoteData } from './utils/remote-data';
import * as fetchMe from './pages/fetch-me';

export type Model = {
  // Data
  user: RemoteData<User, Error>;
  sentTweets: Status[];
  faves: Status[];

  // UI
  pendingTweet: string;
  faveNick: string;
  searchFaves?: string;
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
  // fetchFaves
  | { type: 'start_fetch_faves' }
  | { type: 'receive_fetch_faves'; statuses: Status[] }
  | { type: 'error_fetch_faves'; error: Error }
  // text fields
  | { type: 'update_pending_tweet'; text: string }
  | { type: 'update_fave_nick'; nick: string }
  | { type: 'update_search_faves'; search: string }
  // misc
  | { type: 'add_error'; error: Error }
  | { type: 'clear_error'; error: Error }
  | { type: 'noop' };

export const noop = (): Msg => ({ type: 'noop' });

export const init: Model = {
  user: { type: 'idle' },
  sentTweets: [],
  faves: [],
  pendingTweet: '(initial)',
  faveNick: "(somebody's Twitter ID)",
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
        user: fetchMe.reduce(model.user, msg.sub),
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
    case 'start_fetch_faves': {
      return {
        ...model,
        fetchingFaves: true,
      };
    }
    case 'receive_fetch_faves': {
      return {
        ...model,
        fetchingFaves: false,
        faves: model.faves.concat(msg.statuses.reverse()),
      };
    }
    case 'error_fetch_faves': {
      return {
        ...model,
        fetchingFaves: false,
        errors: model.errors.concat([msg.error]),
      };
    }
    case 'update_pending_tweet':
      return {
        ...model,
        pendingTweet: msg.text,
      };
    case 'update_fave_nick': {
      if (model.user) {
        // Ignore the upate; we will use the nick of the current user.
        return { ...model };
      }
      return {
        ...model,
        faveNick: msg.nick,
      };
    }
    case 'update_search_faves':
      return {
        ...model,
        searchFaves: msg.search,
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
