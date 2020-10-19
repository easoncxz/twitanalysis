import type * as history from 'history';

import { User, Status } from './twitter';
import { typecheckNever, stringEnumValues } from './utils';

export type Model = {
  user?: User;
  pendingTweet: string;
  sentTweets: Status[];
  fetchingMe: boolean;
  sendingTweet: boolean;
};

export type Msg =
  | { type: 'start_fetch_me' }
  | { type: 'receive_fetch_me'; user: User }
  | { type: 'update_pending_tweet'; text: string }
  | { type: 'start_send_tweet' }
  | { type: 'receive_send_tweet'; status: Status }
  | { type: 'noop' };

export const noop = (): Msg => ({ type: 'noop' });

export const init: Model = {
  user: undefined,
  pendingTweet: '(initial)',
  sentTweets: [],
  fetchingMe: false,
  sendingTweet: false,
};

export const reduce = (init: Model) => (
  model: Model | undefined,
  action: Msg,
): Model => {
  if (!model) {
    return init;
  }
  switch (action.type) {
    case 'noop':
      return model;
    case 'start_fetch_me':
      return {
        ...model,
        fetchingMe: true,
      };
    case 'receive_fetch_me':
      return {
        ...model,
        user: action.user,
        fetchingMe: false,
      };
    case 'update_pending_tweet':
      return {
        ...model,
        pendingTweet: action.text,
      };
    case 'start_send_tweet':
      return {
        ...model,
        sendingTweet: true,
      };
    case 'receive_send_tweet': {
      const sentTweets = model.sentTweets.slice();
      sentTweets.push(action.status);
      return {
        ...model,
        sentTweets,
        sendingTweet: false,
      };
    }
    default:
      // Can't just use an ordinary `(n: never) => never` function,
      // because Redux actually abuse our reducer function to run
      // their internal actions. We must return the model despite
      // semantically it's more sensible to throw an error.
      typecheckNever(action);
      return model;
  }
};

export enum Page {
  Home = '/',
  FetchMe = '/fetch-me',
  SendTweet = '/send-tweet',
}

export function parseLocation(
  location: history.Location<unknown>,
): Page | undefined {
  for (const page of stringEnumValues(Page)) {
    if (location.pathname === page) {
      return page;
    }
  }
  return undefined;
}
