'use strict';

import React from 'react';
import ReactDOM from 'react-dom';
import * as Redux from 'redux';
import { createStore, combineReducers, applyMiddleware, compose } from 'redux';
import { Route, Switch } from 'react-router';
import {
  connectRouter,
  routerMiddleware,
  ConnectedRouter,
} from 'connected-react-router';
import * as ConnRouter from 'connected-react-router';
import { createHashHistory } from 'history';
import { Provider } from 'react-redux';

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
};

type Msg =
  | { type: 'noop' }
  | { type: 'fetch_me' }
  | { type: 'receive_me'; user: User }
  | { type: 'update_pending_tweet'; text: string }
  | { type: 'receive_send_tweet_response'; status: Status };

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
        dispatch({ type: 'receive_me', user });
      });
    return {
      type: 'fetch_me',
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
        dispatch({ type: 'receive_send_tweet_response', status });
      });
    return { type: 'noop' };
  },
});

function reducer(model: Model | undefined, action: Msg): Model {
  if (!model) {
    return init;
  }
  switch (action.type) {
    case 'noop':
    case 'fetch_me':
      return model;
    case 'receive_me':
      return {
        ...model,
        user: action.user,
      };
    case 'update_pending_tweet':
      return {
        ...model,
        pendingTweet: action.text,
      };
    case 'receive_send_tweet_response': {
      const sentTweets = model.sentTweets.slice();
      sentTweets.push(action.status);
      return {
        ...model,
        sentTweets,
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

const store: Redux.Store<ModelAndRouter, MsgOrRouter> = createStore(
  combineReducers({
    model: reducer,
    router: connectRouter(hist),
  }),
  undefined,
  compose(applyMiddleware(routerMiddleware(hist))),
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
          return (
            <button onClick={() => dispatch(actions.fetchMe())}>
              Tell me who I am
            </button>
          );
        }
      })()
    }

    <h2>Send tweet</h2>
    <form action="#">
      <textarea
        value={model.pendingTweet}
        onChange={(e) => {
          dispatch({ type: 'update_pending_tweet', text: e.target.value });
        }}
      ></textarea>
      <br />
      <pre>{model.pendingTweet}</pre>
      <button onClick={() => dispatch(actions.sendTweet(model.pendingTweet))}>
        Send this tweet
      </button>
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

const mountPoint = document.getElementById('react-mountpoint');
store.subscribe(() => {
  ReactDOM.render(
    <Provider store={store}>
      <ConnectedRouter history={hist}>
        <>
          <Switch>
            <Route exact path="/" render={app} />
            <Route
              exact
              path="/other"
              render={() => (
                <div>
                  <p>Another path</p>
                  <p>The current location is:</p>
                  <pre>{pretty(store.getState().router.location)}</pre>
                  <button onClick={() => store.dispatch(ConnRouter.push('/'))}>
                    Go home
                  </button>
                </div>
              )}
            />
          </Switch>
        </>
      </ConnectedRouter>
    </Provider>,
    mountPoint,
  );
});

console.log('main.tsx here.');
if (
  typeof window === 'object' &&
  typeof window.addEventListener === 'function'
) {
  window.addEventListener('load', () => store.dispatch({ type: 'noop' }));
}
