import { User, Status } from './twitter/models';
import { typecheckNever } from './utils/utils';

export type Model = {
  // Data
  user?: User;
  sentTweets: Status[];
  faves: Status[];

  // UI
  pendingTweet: string;
  faveNick: string;
  searchFaves?: string;
  errors: Error[];

  // Network state
  fetchingMe: boolean;
  fetchingFaves: boolean;
  sendingTweet: boolean;
};

export type Msg =
  // fetchMe
  | { type: 'start_fetch_me' }
  | { type: 'receive_fetch_me'; user: User }
  | { type: 'error_fetch_me'; error: Error }
  // sendTweet
  | { type: 'start_send_tweet' }
  | { type: 'receive_send_tweet'; status: Status }
  | { type: 'error_send_tweet'; error: Error }
  // fetchFaves
  | { type: 'start_fetch_faves' }
  | { type: 'receive_fetch_faves'; statuses: Status[] }
  | { type: 'error_fetch_faves'; error: Error }
  // idb general
  | { type: 'idb_creating_db' }
  | { type: 'idb_created_db' }
  | { type: 'idb_deleting_db' }
  | { type: 'idb_deleted_db' }
  // load-store user
  | { type: 'idb_saving_user'; user: User }
  | { type: 'idb_saved_user'; user: User }
  | { type: 'idb_reading_user'; id_str: string }
  | { type: 'idb_read_user'; id_str: string; user?: User }
  // load-store tweets
  | { type: 'idb_saving_tweet'; status: Status }
  | { type: 'idb_saved_tweet'; status: Status }
  | { type: 'idb_saving_tweets'; statuses: Status[] }
  | { type: 'idb_saved_tweets'; statuses: Status[] }
  | { type: 'idb_reading_tweet'; id_str: string }
  | { type: 'idb_read_tweet'; id_str: string; status?: Status }
  | { type: 'idb_reading_tweets_slice'; start: number; end: number }
  | {
      type: 'idb_read_tweets_slice';
      start: number;
      end: number;
      statuses: Status[];
    }
  | { type: 'idb_reading_all_tweets' }
  | { type: 'idb_read_all_tweets'; statuses: Status[] }
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
  user: undefined,
  sentTweets: [],
  faves: [],
  pendingTweet: '(initial)',
  faveNick: "(somebody's Twitter ID)",
  errors: [],
  fetchingMe: false,
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
    case 'start_fetch_me':
      return {
        ...model,
        fetchingMe: true,
      };
    case 'receive_fetch_me':
      return {
        ...model,
        user: msg.user,
        fetchingMe: false,
      };
    case 'error_fetch_me': {
      return {
        ...model,
        fetchingMe: false,
        errors: model.errors.concat([msg.error]),
      };
    }
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
    case 'idb_creating_db':
    case 'idb_created_db':
    case 'idb_deleting_db':
    case 'idb_deleted_db':
      console.log(msg);
      return { ...model };

    case 'idb_saving_user':
    case 'idb_saved_user':
    case 'idb_reading_user':
    case 'idb_read_user':
      console.log(msg);
      return { ...model };

    case 'idb_saving_tweet':
    case 'idb_saved_tweet':
    case 'idb_saving_tweets':
    case 'idb_saved_tweets':
    case 'idb_reading_tweet':
    case 'idb_read_tweet':
    case 'idb_reading_tweets_slice':
    case 'idb_read_tweets_slice':
      console.log(msg);
      return { ...model }; // do nothing

    case 'idb_reading_all_tweets':
      console.log(msg);
      return { ...model }; // do nothing
    case 'idb_read_all_tweets':
      console.log(msg);
      return { ...model, faves: model.faves.concat(msg.statuses) };

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
