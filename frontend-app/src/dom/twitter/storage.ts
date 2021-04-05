import { openDB, deleteDB } from 'idb';
import type { IDBPDatabase, IDBPTransaction, DBSchema } from 'idb';

import * as twitter from './models';

interface TwitDb extends DBSchema {
  tweets: {
    key: string;
    value: twitter.Status;
  };
  users: {
    key: string;
    value: twitter.User;
  };
  lists: {
    key: string;
    value: twitter.List;
  };
}

const dbName = 'twitanalysis-idb';
const currentVersion = 2;

export async function openMyDB(): Promise<IDBPDatabase<TwitDb>> {
  const db: IDBPDatabase<TwitDb> = await openDB<TwitDb>(
    dbName,
    currentVersion,
    {
      blocked() {
        console.log('openDB Blocked');
      },
      terminated() {
        console.log("We're sad to see you go.");
      },
      blocking() {
        console.log("Sorry, just a moment and we'll head off");
      },
      upgrade(
        db: IDBPDatabase<TwitDb>,
        oldVersion: number,
        _newVersion: number,
        tx: IDBPTransaction<TwitDb>,
      ) {
        if (oldVersion < 1) {
          console.log('Inside openDB.upgrade');
          db.createObjectStore('tweets', {
            keyPath: 'id_str',
          });
          db.createObjectStore('users', {
            keyPath: 'id_str',
          });
        }
        if (oldVersion < 2) {
          db.createObjectStore('lists', {
            keyPath: 'id_str',
          });
        }
        return tx.done;
      },
    },
  );
  return db;
}

export async function deleteMyDb(): Promise<void> {
  await deleteDB(dbName, {
    blocked() {
      console.log(`deleteDB blocked`);
    },
  });
}

export async function withDB<T>(
  action: (_: IDBPDatabase<TwitDb>) => Promise<T>,
): Promise<T> {
  return openMyDB().then(action);
}

// ------
// a bunch of single-transaction actions follows
// ------

export function saveUser(user: twitter.User): Promise<void> {
  return withDB(async (db) => {
    const tx = db.transaction('users', 'readwrite');
    const users = tx.objectStore('users');
    await users.add(user);
    return tx.done;
  });
}

export function readUser(id_str: string): Promise<twitter.User | undefined> {
  return withDB(async (db) => {
    const tx = db.transaction('users');
    const users = tx.store;
    return users.get(id_str);
  });
}

/**
 * Seems to return stored Status ID
 */
export function putTweet(s: twitter.Status): Promise<string> {
  return withDB(async (db) => {
    const tx = db.transaction('tweets', 'readwrite');
    const tweets = tx.objectStore('tweets');
    return tweets.put(s);
  });
}

export function putTweets(ss: twitter.Status[]): Promise<string[]> {
  return withDB(async (db) => {
    const tx = db.transaction('tweets', 'readwrite');
    const tweets = () => tx.objectStore('tweets');
    const result = await Promise.all(ss.map((s) => tweets().put(s)));
    await tx.done;
    return result;
  });
}

export function readTweet(id_str: string): Promise<twitter.Status | undefined> {
  return withDB(async (db) => {
    const tx = db.transaction('tweets');
    const tweets = tx.objectStore('tweets');
    return tweets.get(id_str);
  });
}

export function readAllTweets(): Promise<twitter.Status[]> {
  return withDB(async (db) => {
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
  });
}

export function storeLists(ls: twitter.List[]): Promise<string[]> {
  return withDB(async (db) => {
    const tx = db.transaction('lists');
    const lists = tx.objectStore('lists');
    const result = await Promise.all(ls.map((l) => lists.put(l)));
    await tx.done;
    return result;
  });
}

export function readLists(): Promise<twitter.List[]> {
  return withDB(async (db) => {
    const tx = db.transaction('lists');
    const lists = tx.store;
    return lists.getAll();
  });
}
