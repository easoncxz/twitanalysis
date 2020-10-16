'use strict';

import * as ConnRouter from 'connected-react-router';
import * as ReactRedux from 'react-redux';
import * as Redux from 'redux';
import React from 'react';
import ReactDOM from 'react-dom';
import { ConnectedRouter } from 'connected-react-router';
import { createHashHistory } from 'history';

/**
 * Returns any type, instead of returning `never`,
 * in order to not create "unreachable" code.
 */
function typecheckNever<T>(n: never): T {
  return n;
}

function t(rel: string): string {
  return '/to-twitter/' + rel;
}

type User = {
  id_str: string;
  name: string;
  screen_name: string;
};

type Status = {
  id_str: string;
  text: string;
  created_at: string;
};

type Model = {
  user?: User;
  pendingTweet: string;
  sentTweets: Status[];
  fetchingMe: boolean;
  sendingTweet: boolean;
};

function prettyUser(user: User): string {
  return JSON.stringify(user, undefined, 4);
}

function pretty(o: unknown): string {
  return JSON.stringify(o, undefined, 4);
}

const init: Model = {
  user: undefined,
  pendingTweet: '(initial)',
  sentTweets: [],
  fetchingMe: false,
  sendingTweet: false,
};

type Msg =
  | { type: 'start_fetch_me' }
  | { type: 'receive_fetch_me'; user: User }
  | { type: 'update_pending_tweet'; text: string }
  | { type: 'start_send_tweet' }
  | { type: 'receive_send_tweet'; status: Status }
  | { type: 'noop' };

type Actions = {
  noop(): Msg;
  fetchMe(): Msg;
  sendTweet(t: string): Msg;
};

const actionsOf = (dispatch: Redux.Dispatch<Msg>): Actions => ({
  noop() {
    return {
      type: 'noop',
    };
  },
  fetchMe() {
    fetch(t('account/verify_credentials.json'))
      .then((r) => r.json())
      .then((user: User) => {
        dispatch({ type: 'receive_fetch_me', user });
      });
    return {
      type: 'start_fetch_me',
    };
  },
  sendTweet(text: string) {
    const body = new URLSearchParams();
    body.set('status', text);
    fetch(t('statuses/update.json'), {
      method: 'POST',
      headers: {
        'Content-Type': 'application/x-www-form-urlencoded',
      },
      body,
    })
      .then((r) => r.json())
      .then((status: Status) => {
        dispatch({ type: 'receive_send_tweet', status });
      });
    return { type: 'start_send_tweet' };
  },
});

function reducer(model: Model | undefined, action: Msg): Model {
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
}

type ModelAndRouter = Redux.CombinedState<{
  model: Model;
  router: ConnRouter.RouterState<unknown>;
}>;

type MsgOrRouter = Msg | ConnRouter.RouterAction<unknown>;

const hist = createHashHistory();

const store: Redux.Store<ModelAndRouter, MsgOrRouter> = Redux.createStore(
  Redux.combineReducers({
    model: reducer,
    router: ConnRouter.connectRouter(hist),
  }),
  undefined,
  Redux.compose(Redux.applyMiddleware(ConnRouter.routerMiddleware(hist))),
);

const App: React.FunctionComponent<{
  model: Model;
  dispatch: Redux.Dispatch<Msg>;
  actions: Actions;
}> = ({ model, dispatch, actions }) => (
  <div>
    <h1>Hello from React</h1>

    <h1>Fetch own user</h1>
    {
      // Fetch user
      (() => {
        if (model.user) {
          return (
            <div>
              <p>You are:</p>
              <pre>{prettyUser(model.user)}</pre>
            </div>
          );
        } else {
          const b = (disabled: boolean) => (
            <button
              onClick={() => dispatch(actions.fetchMe())}
              disabled={disabled}
            >
              Tell me who I am
            </button>
          );
          if (model.fetchingMe) {
            return (
              <div>
                <p>Fetching...</p>
                {b(true)}
              </div>
            );
          } else {
            return b(false);
          }
        }
      })()
    }

    <h2>Send tweet</h2>
    <form action="">
      <textarea
        value={model.pendingTweet}
        onChange={(e) => {
          dispatch({ type: 'update_pending_tweet', text: e.target.value });
        }}
        disabled={model.sendingTweet}
      ></textarea>
      <br />
      <pre>{model.pendingTweet}</pre>
      <input
        type="submit"
        onClick={(e) => {
          e.preventDefault();
          dispatch(actions.sendTweet(model.pendingTweet));
        }}
        disabled={model.sendingTweet}
        value="Send this tweet"
      />
    </form>

    <p>Sent tweets:</p>
    <ul>
      {model.sentTweets.map((st, i) => (
        <li key={`sentTweets-${i}`}>
          <code>{st.created_at}</code> - {st.text}
        </li>
      ))}
    </ul>
  </div>
);

const app = () => (
  <App
    model={store.getState().model}
    dispatch={store.dispatch}
    actions={actionsOf(store.dispatch)}
  />
);
void app;

function routing() {
  const loc = store.getState().router.location;
  switch (loc.pathname) {
    case '/':
      return <p>Root</p>;
    case '/other':
      return (
        <div>
          <p>Other</p>
          <pre>{pretty(loc)}</pre>
        </div>
      );
    case '/app':
      return app();
    default:
      return <p>Default</p>;
  }
}

const mountpoint = document.getElementById('react-mountpoint');
store.subscribe(() => {
  ReactDOM.render(routing(), mountpoint);
});

console.log('main.tsx here.');
if (
  typeof window === 'object' &&
  typeof window.addEventListener === 'function'
) {
  window.addEventListener('load', () => {
    // ??? Just connecting up the `hist` with the store with
    // some event listeners. I'd rather not render at all.
    ReactDOM.render(
      <ReactRedux.Provider store={store}>
        <ConnectedRouter history={hist}></ConnectedRouter>
      </ReactRedux.Provider>,
      document.getElementById('router-mountpoint'),
    );
    store.dispatch({ type: 'noop' });
  });
}
