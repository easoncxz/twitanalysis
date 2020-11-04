import { openDB, deleteDB } from 'idb';
import type { IDBPDatabase, IDBPTransaction, DBSchema } from 'idb';

import { User, Status } from './twitter';

interface TwitDb extends DBSchema {
  tweets: {
    key: string;
    value: Status;
  };
  users: {
    key: string;
    value: User;
  };
}

const dbName = 'twitanalysis-idb';
const dbVersion = 1;

export async function openMyDB(): Promise<IDBPDatabase<TwitDb>> {
  const db: IDBPDatabase<TwitDb> = await openDB<TwitDb>(dbName, dbVersion, {
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
      _oldVersion: number,
      _newVersion: number,
      tx: IDBPTransaction<TwitDb>,
    ) {
      console.log('Inside openDB.upgrade');
      db.createObjectStore('tweets', {
        keyPath: 'id_str',
      });
      db.createObjectStore('users', {
        keyPath: 'id_str',
      });
      return tx.done;
    },
  });
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
