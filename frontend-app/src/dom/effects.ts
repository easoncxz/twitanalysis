import type { Dispatch } from 'redux';

import * as core from './core';
import * as tdb from './twit-db';
import { User, Status, t, parseUser, parseStatus, parseArray } from './twitter';

export type Effects = {
  noop(): core.Msg;
  fetchMe(): core.Msg;
  fetchFaves(
    nik: string,
    _: { count?: number; max_id?: string; since_id?: string },
  ): core.Msg;
  sendTweet(t: string): core.Msg;
  createTwitDb(): core.Msg;
  deleteTwitDb(): core.Msg;
  saveUser(u: User): core.Msg;
  readUser(id_str: string): core.Msg;
  putTweet(s: Status): core.Msg;
  putTweets(ss: Status[]): core.Msg;
  readTweet(id_str: string): core.Msg;
  readAllTweets(): core.Msg;

  registerServiceWorker(): core.Msg;
  unregisterAllServiceWorkers(): core.Msg;
};

function guardOk(r: Response): Response {
  if (!(200 <= r.status && r.status < 300)) {
    throw new Error(`Request not ok: ${r.status}`);
  } else {
    return r;
  }
}

export async function fetchJson(
  url: string,
  init?: RequestInit,
): Promise<unknown> {
  return fetch(url, init)
    .then(guardOk)
    .then((r) => r.json());
}

export const effectsOf = (dispatch: Dispatch<core.Msg>): Effects => {
  return {
    noop() {
      return {
        type: 'noop',
      };
    },

    fetchMe() {
      fetchJson(t('account/verify_credentials'))
        .then(parseUser)
        .then(
          (user: User) => {
            dispatch({ type: 'receive_fetch_me', user });
          },
          (e) => dispatch({ type: 'error_fetch_me', error: e }),
        );
      return {
        type: 'start_fetch_me',
      };
    },

    fetchFaves(nik, { count = 200, since_id, max_id }) {
      const searchParams = new URLSearchParams();
      searchParams.append('screen_name', nik);
      searchParams.append('count', count.toString());
      if (since_id !== undefined) {
        searchParams.append('since_id', since_id);
      }
      if (max_id !== undefined) {
        searchParams.append('max_id', max_id);
      }
      fetchJson(t('favorites/list') + '?' + searchParams.toString())
        .then(parseArray(parseStatus))
        .then(
          (statuses: Status[]) => {
            dispatch({ type: 'receive_fetch_faves', statuses });
          },
          (e) => dispatch({ type: 'error_fetch_faves', error: e }),
        );
      return {
        type: 'start_fetch_faves',
      };
    },

    sendTweet(text: string) {
      const body = new URLSearchParams();
      body.set('status', text);
      fetchJson(t('statuses/update'), {
        method: 'POST',
        headers: {
          'Content-Type': 'application/x-www-form-urlencoded',
        },
        body,
      })
        .then(parseStatus)
        .then(
          (status: Status) => {
            dispatch({ type: 'receive_send_tweet', status });
          },
          (e) => dispatch({ type: 'error_send_tweet', error: e }),
        );
      return { type: 'start_send_tweet' };
    },

    createTwitDb() {
      tdb.openMyDB().then(() => dispatch({ type: 'idb_created_db' }));
      return { type: 'idb_creating_db' };
    },

    deleteTwitDb() {
      tdb.deleteMyDb();
      return core.noop();
    },

    saveUser(user) {
      tdb
        .withDB(async (db) => {
          const tx = db.transaction('users', 'readwrite');
          const users = tx.objectStore('users');
          await users.add(user);
          return tx.done;
        })
        .then(() => {
          dispatch({ type: 'idb_saved_user', user });
        });
      return { type: 'idb_saving_user', user };
    },

    readUser(id_str) {
      tdb
        .withDB(async (db) => {
          const tx = db.transaction('users');
          const users = tx.store;
          return users.get(id_str);
        })
        .then((user) => {
          dispatch({ type: 'idb_read_user', id_str, user });
        });
      return { type: 'idb_reading_user', id_str };
    },

    putTweet(s: Status) {
      tdb
        .withDB(async (db) => {
          const tx = db.transaction('tweets', 'readwrite');
          const tweets = tx.objectStore('tweets');
          return tweets.put(s);
        })
        .then(() => dispatch({ type: 'idb_saved_tweet', status: s }));
      return { type: 'idb_saving_tweet', status: s };
    },

    putTweets(ss: Status[]) {
      tdb
        .withDB(async (db) => {
          const tx = db.transaction('tweets', 'readwrite');
          const tweets = () => tx.objectStore('tweets');
          const result = await Promise.all(ss.map((s) => tweets().put(s)));
          await tx.done;
          return result;
        })
        .then(() => dispatch({ type: 'idb_saved_tweets', statuses: ss }));
      return { type: 'idb_saving_tweets', statuses: ss };
    },

    readTweet(id_str: string) {
      tdb
        .withDB(async (db) => {
          const tx = db.transaction('tweets');
          const tweets = tx.objectStore('tweets');
          return tweets.get(id_str);
        })
        .then((status) => dispatch({ type: 'idb_read_tweet', id_str, status }));
      return { type: 'idb_reading_tweet', id_str };
    },

    readAllTweets() {
      tdb
        .withDB(async (db) => {
          const tx = db.transaction('tweets');
          const tweets = tx.objectStore('tweets');
          const res = [];
          let cursor = await tweets.openCursor(undefined, 'prev');
          while (cursor) {
            res.push(cursor.value);
            cursor = await cursor.continue();
          }
          await tx.done;
          return res;
        })
        .then((statuses) =>
          dispatch({ type: 'idb_read_all_tweets', statuses }),
        );
      return { type: 'idb_reading_all_tweets' };
    },

    registerServiceWorker() {
      (async () => {
        if ('serviceWorker' in navigator) {
          try {
            const reg = await navigator.serviceWorker.register('/sw.js');
            console.log(
              'From main.js: ServiceWorker registration complete:',
              reg,
              reg.scope,
            );
          } catch (e) {
            console.log('From main.js: ServiceWorker registration failed:', e);
            dispatch({ type: 'add_error', error: e });
          }
        } else {
          console.log("Main.ts: ServiceWorker doesn't appear to be supported.");
        }
      })();
      return this.noop();
    },

    unregisterAllServiceWorkers() {
      (async () => {
        if ('serviceWorker' in navigator) {
          try {
            const regs = await navigator.serviceWorker.getRegistrations();
            for (const r of regs) {
              const ok = await r.unregister();
              if (!ok) {
                throw new Error('unregister not ok');
              }
            }
          } catch (e) {
            console.warn(`Main.ts: ServiceWorker unregisteration failed: ${e}`);
            dispatch({ type: 'add_error', error: e });
          }
        } else {
          console.log("Main.ts: ServiceWorker doesn't appear to be supported.");
        }
      })();
      return this.noop();
    },
  };
};
