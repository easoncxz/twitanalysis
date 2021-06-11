import React, { FC } from 'react';

import * as twitter from '../twitter/models';
import { t } from '../twitter/models';
import * as tdb from '../twitter/storage';
import { RemoteData } from '../utils/remote-data';
import * as remoteData from '../utils/remote-data';
import {
  fetchJson,
  typecheckNever,
  MaybeDefined,
  mapMaybe,
  params,
  defOpts,
} from '../utils/utils';

type MyDispatch<T> = (_: T) => T;

type ListAndMembers = {
  list: twitter.List;
  members: RemoteData<twitter.User[], Error>;
};

type UserAndTweets = {
  user: twitter.User;
  tweets: RemoteData<twitter.Status[], Error>;
};

export type Model = {
  allLists: RemoteData<twitter.List[], Error>;
  focusedList: MaybeDefined<ListAndMembers>;
  userFocus: MaybeDefined<UserAndTweets>;
};

export const init: Model = {
  allLists: { type: 'idle' },
  focusedList: undefined,
  userFocus: undefined,
};

export type Msg =
  | { type: 'noop' }
  | {
      type: 'focus_list';
      focusIdStr: string;
    }
  | { type: 'focus_user'; user: MaybeDefined<twitter.User> }
  | {
      type: 'fetch_lists';
      update: RemoteData<twitter.List[], Error>;
    }
  | {
      type: 'fetch_list_members';
      listIdStr: string;
      members: RemoteData<twitter.User[], Error>;
    }
  | {
      type: 'fetch_user_timeline';
      userIdStr: string;
      tweets: RemoteData<twitter.Status[], Error>;
    };

export const reduce = (init: Model) => (
  model: Model | undefined,
  msg: Msg,
): Model => {
  if (model === undefined) {
    return init;
  }
  switch (msg.type) {
    case 'noop':
      return model;
    case 'fetch_lists': {
      const focusedList: MaybeDefined<ListAndMembers> = mapMaybe(
        (l: twitter.List) => ({
          list: l,
          // Oh my god TypeScript's type-inference is so incapable!
          // It's probably because of JavaScript's subtyping.
          members: remoteData.idle<twitter.User[], Error>(),
        }),
        remoteData.toMaybeDefined(msg.update)?.[0],
      );
      console.log(`Focusing on list: ${focusedList?.list.name}`);
      // Here we kind of want something like redux-loop so that we can
      // fire off another effect to fetch members of this list. For now,
      // we do it via React useEffect or even just a button in the UI.
      return {
        ...model,
        allLists: remoteData.reduce(model.allLists, msg.update),
        focusedList,
      };
    }
    case 'focus_list': {
      const list: MaybeDefined<twitter.List> = remoteData.toMaybeDefined(
        remoteData.map((ls: twitter.List[]) => {
          return ls.filter((l) => l.id_str === msg.focusIdStr)[0];
        })(model.allLists),
      );
      const focusedList = mapMaybe(
        (l) => ({
          list: l,
          members: remoteData.idle<twitter.User[], Error>(),
        }),
        list,
      );
      console.log(`Focusing on list: ${focusedList?.list.name}`);
      return {
        ...model,
        focusedList,
      };
    }

    case 'focus_user': {
      return {
        ...model,
        userFocus: mapMaybe(
          (u) => ({
            user: u,
            tweets: remoteData.idle<twitter.Status[]>(),
          }),
          msg.user,
        ),
      };
    }
    case 'fetch_list_members': {
      if (model.focusedList === undefined) {
        // No focused list; ignoring any incoming messages
        return model;
      } else if (model.focusedList.list.id_str !== msg.listIdStr) {
        // The response came too late. We now ignore it.
        return model;
      } else {
        return {
          ...model,
          focusedList: {
            ...model.focusedList,
            members: remoteData.reduce(model.focusedList?.members, msg.members),
          },
        };
      }
    }
    case 'fetch_user_timeline': {
      if (model.userFocus === undefined) {
        // no focused user; ignoring incoming messages
        return model;
      } else if (model.userFocus.user.id_str !== msg.userIdStr) {
        // The repsonse is for a different user. We now ignore it.
        // Although, if we can dispatch an action here, it would be
        // better to save it to idb anyway.
        return model;
      } else {
        return {
          ...model,
          userFocus: {
            ...model.userFocus,
            tweets: remoteData.reduce(model.userFocus.tweets, msg.tweets),
          },
        };
      }
    }
    default:
      typecheckNever(msg);
      return model;
  }
};

const compareOn = <T, K extends keyof T>(key: K) => (l: T, r: T): number => {
  if (l[key] < r[key]) {
    return -1;
  } else if (l[key] > r[key]) {
    return 1;
  } else {
    return 0;
  }
};

export class Effects {
  constructor(private readonly dispatch: MyDispatch<Msg>) {}

  loadListsFromIdb<T>(cont?: (_: twitter.List[]) => T): Msg {
    tdb.readLists().then(
      (lists: twitter.List[]) => {
        const sorted = lists.slice().sort(compareOn('name'));
        this.dispatch({
          type: 'fetch_lists',
          update: {
            type: 'ok',
            data: sorted,
          },
        });
        cont?.(sorted);
      },
      (e) => {
        console.error(`tdb.readLists failed: ${e.message}`, e);
        this.dispatch({
          type: 'fetch_lists',
          update: { type: 'error', error: e },
        });
      },
    );
    return {
      type: 'fetch_lists',
      update: { type: 'loading' },
    };
  }

  fetchListsFromNetwork(): Msg {
    fetchJson(
      t('lists/list') + '?reverse=true',
      undefined,
      twitter.parseArray(twitter.parseList),
    )
      .then(
        (lists: twitter.List[]) => {
          this.dispatch({
            type: 'fetch_lists',
            update: {
              type: 'ok',
              data: lists
                .slice()
                .sort((l, r) =>
                  l.name < r.name ? -1 : l.name > r.name ? 1 : 0,
                ),
            },
          });
          return lists;
        },
        (e) => {
          console.error(`fetchJson failed when loading lists: ${e.message}`);
          this.dispatch({
            type: 'fetch_lists',
            update: { type: 'error', error: e },
          });
        },
      )
      .then((lists) => {
        if (lists) {
          tdb.storeLists(lists);
        }
        return lists;
      });
    return {
      type: 'fetch_lists',
      update: { type: 'loading' },
    };
  }

  addMemberToList(userIdStr: string, listIdStr: string): Msg {
    fetchJson(t('lists/members/create'), {
      method: 'POST',
      body: params([
        ['list_id', listIdStr],
        ['user_id', userIdStr],
      ]),
      headers: {
        'Content-Type': 'application/x-www-form-urlencoded',
      },
    }).then(
      (resp) => {
        console.log(`Added user ${userIdStr} to list ${listIdStr}`);
        void resp;
      },
      (err) => {
        console.log(`Failed to add user ${userIdStr} to list ${listIdStr}`);
        void err;
      },
    );
    return { type: 'noop' };
  }

  fetchListMembersFromNetwork(listIdStr: string): Msg {
    return {
      type: 'fetch_list_members',
      listIdStr,
      members: remoteData.launchPromise<twitter.User[], Error>(
        (rd: RemoteData<twitter.User[], Error>) => {
          this.dispatch({
            type: 'fetch_list_members',
            listIdStr,
            members: rd,
          });
          // Do one more thing when this action finishes;
          // again, this probably is begging for redux-loop.
          if (rd.type === 'ok') {
            rd.data.forEach((user) => {
              tdb.saveUser(user);
            });
          }
        },
        () =>
          fetchJson(
            t('lists/members') +
              '?' +
              params([['list_id', listIdStr]]).toString(),
            undefined,
            twitter.parseListMembersResponse,
          ).then((r) => r.users), // TODO: this is unsafe -- we want all pages
      ),
    };
  }

  saveListMembershipsToIdb(
    listIdStr: string,
    users: RemoteData<twitter.User[], Error>,
  ): Msg {
    switch (users.type) {
      case 'idle':
        window.alert(`Please fetch from network first, by clicking "fetch"`);
        return { type: 'noop' };
      case 'loading':
        window.alert(`Still loading, please wait.`);
        return { type: 'noop' };
      case 'ok': {
        const memberships: twitter.ListMembership[] = users.data.map((u) => ({
          userIdStr: u.id_str,
          listIdStr,
        }));
        tdb.addListMemberships(memberships).then(
          () => {
            console.log(`saveListMembershipsToIdb success`);
          },
          (e) => {
            console.error(`saveListMembershipsToIdb error: ${e.message}`);
          },
        );
        return { type: 'noop' };
      }
      case 'error':
        window.alert(
          `There was an error as you can see. Please fetch again first.`,
        );
        return { type: 'noop' };
      default:
        typecheckNever(users);
        return { type: 'noop' };
    }
  }

  loadListMembersFromIdb(listIdStr: string): Msg {
    return {
      type: 'fetch_list_members',
      listIdStr,
      members: remoteData.launchPromise<twitter.User[], Error>(
        (us) =>
          this.dispatch({ type: 'fetch_list_members', listIdStr, members: us }),
        () => tdb.loadListMembers(listIdStr),
      ),
    };
  }

  fetchUserTimeline(
    userIdStr: string,
    opts?: {
      maxId?: string;
      sinceId?: string;
    },
  ): Msg {
    const { maxId, sinceId } = defOpts(opts);
    const pagingParams = ([] as [string, string][])
      .concat(maxId ? [['max_id', maxId]] : [])
      .concat(sinceId ? [['since_id', sinceId]] : []);
    const tweets = remoteData.launchPromise<twitter.Status[]>(
      (ts) => {
        this.dispatch({
          type: 'fetch_user_timeline',
          userIdStr,
          tweets: ts,
        });
      },
      () =>
        fetchJson(
          t('statuses/user_timeline') +
            '?' +
            params([['user_id', userIdStr], ...pagingParams]).toString(),
        ),
    );
    return {
      type: 'fetch_user_timeline',
      userIdStr,
      tweets,
    };
  }

  saveUserTweets(userIdStr: string, tweets: twitter.Status[]): Msg {
    // no UI updates
    void userIdStr;
    tdb.putTweets(tweets);
    return { type: 'noop' };
  }

  loadUserTweets(userIdStr: string): Msg {
    return {
      type: 'fetch_user_timeline',
      userIdStr,
      tweets: remoteData.launchPromise<twitter.Status[]>(
        (ts) => {
          this.dispatch({
            type: 'fetch_user_timeline',
            userIdStr,
            tweets: ts,
          });
        },
        () => tdb.readUserTweets(userIdStr),
      ),
    };
  }
}

export type Props = {
  model: Model;
  dispatch: MyDispatch<Msg>;
};

const ListPicker: FC<Props> = ({ model, dispatch }) => {
  const effects = new Effects(dispatch);
  return (
    <div className="list-picker">
      {(() => {
        switch (model.allLists.type) {
          case 'idle':
            return (
              <div className="select">{`Click "fetch" to fetch lists.`}</div>
            );
          case 'loading':
            return <div className="select">Loading...</div>;
          case 'ok':
            return (
              <select
                value={model.focusedList?.list.id_str}
                onChange={(e) => {
                  dispatch({ type: 'focus_list', focusIdStr: e.target.value });
                }}
              >
                {model.allLists.data.map((l: twitter.List) => (
                  <option key={l.id_str} value={l.id_str}>
                    {l.name}
                  </option>
                ))}
              </select>
            );
          case 'error':
            return (
              <div className="select">
                An error occurred.
                <code>
                  {JSON.stringify(model.allLists.error, undefined, 4)}
                </code>
              </div>
            );
          default:
            return typecheckNever(model.allLists);
        }
      })()}
      {(model.allLists.type === 'idle' || model.allLists.type === 'error') && (
        <>
          <button
            type="button"
            onClick={() => {
              dispatch(effects.fetchListsFromNetwork());
            }}
          >
            fetch and save
          </button>
          <button
            type="button"
            onClick={() => {
              dispatch(effects.loadListsFromIdb());
            }}
          >
            load
          </button>
        </>
      )}
    </div>
  );
};

const ListMembersView: FC<{
  focusedList: MaybeDefined<ListAndMembers>;
  userFocus: MaybeDefined<UserAndTweets>;
  dispatch: MyDispatch<Msg>;
}> = ({ focusedList, userFocus, dispatch }) => {
  const effects = new Effects(dispatch);
  if (focusedList === undefined) {
    return <p>No list in focus</p>;
  } else {
    const { list, members } = focusedList;
    return (
      <>
        <div>
          <button
            type="button"
            onClick={() =>
              dispatch(effects.fetchListMembersFromNetwork(list.id_str))
            }
          >
            fetch
          </button>
          <button
            type="button"
            onClick={() => {
              dispatch(effects.saveListMembershipsToIdb(list.id_str, members));
            }}
          >
            save
          </button>
          <button
            type="button"
            onClick={() => {
              dispatch(effects.loadListMembersFromIdb(list.id_str));
            }}
          >
            load
          </button>
        </div>
        {(() => {
          switch (members.type) {
            case 'idle':
              return <p>Members not yet known.</p>;
            case 'loading':
              return <p>Loading list members...</p>;
            case 'ok': {
              const users: twitter.User[] = members.data;
              return (
                <ul>
                  {users.map((u) => {
                    const isTheFocus = u.id_str === userFocus?.user.id_str;
                    const newFocus = isTheFocus ? undefined : u;
                    const classNameProps = isTheFocus
                      ? { className: 'source-list-user focused' }
                      : { className: 'source-list-user' };
                    return (
                      <li {...classNameProps} key={u.id_str}>
                        <button
                          type="button"
                          onClick={() => {
                            dispatch({ type: 'focus_user', user: newFocus });
                          }}
                        >
                          {u.screen_name}
                        </button>
                      </li>
                    );
                  })}
                </ul>
              );
            }
            case 'error':
              return <p>Error fetching list members.</p>;
            default:
              typecheckNever(members);
              return members;
          }
        })()}
      </>
    );
  }
};

const Timeline: FC<{
  user: twitter.User;
  tweets: twitter.Status[];
  dispatch: MyDispatch<Msg>;
}> = ({ tweets }) => {
  return (
    <ul>
      {tweets.map((t) => (
        <li key={t.id_str}>{t.text}</li>
      ))}
    </ul>
  );
};

const FocusedUser: FC<{
  userFocus: MaybeDefined<UserAndTweets>;
  dispatch: MyDispatch<Msg>;
}> = ({ userFocus, dispatch }) => {
  const effects = new Effects(dispatch);
  if (userFocus === undefined) {
    return <p>Click a user on the left.</p>;
  } else {
    const { user, tweets } = userFocus;
    return (
      <>
        <p>{`You're focused on ${user.name}`}</p>
        <div>
          <button
            type="button"
            onClick={() => {
              dispatch(effects.fetchUserTimeline(user.id_str));
            }}
          >
            fetch
          </button>
          <button
            type="button"
            onClick={() => {
              if (tweets.type === 'ok') {
                console.log(
                  `Saving ${tweets.data.length} tweets for user ${user.name}`,
                );
                dispatch(effects.saveUserTweets(user.id_str, tweets.data));
              } else {
                console.log(
                  `No tweets available to save for user ${user.name}`,
                );
              }
            }}
          >
            save
          </button>
          <button
            type="button"
            onClick={() => {
              console.log(`Loading tweets for user ${user.name}`);
              dispatch(effects.loadUserTweets(user.id_str));
            }}
          >
            load
          </button>
          {remoteData.simpleView(userFocus.tweets, (ts) => (
            <Timeline user={userFocus.user} tweets={ts} dispatch={dispatch} />
          ))}
        </div>
      </>
    );
  }
};

export const DestinationListList: FC<Props> = ({ model, dispatch }) => {
  const effects = new Effects(dispatch);
  return (
    <>
      Destination list:
      {(() => {
        switch (model.allLists.type) {
          case 'idle':
            return (
              <div className="select">{`Click "fetch" to fetch lists.`}</div>
            );
          case 'loading':
            return <div className="select">Loading...</div>;
          case 'ok':
            return (
              <ul>
                {model.allLists.data.map((l: twitter.List) => (
                  <li key={l.id_str}>
                    <div>
                      {l.name}
                      <button
                        type="button"
                        onClick={() => {
                          if (model.userFocus !== undefined) {
                            effects.addMemberToList(
                              model.userFocus.user.id_str,
                              l.id_str,
                            );
                          }
                        }}
                      >
                        {'+'}
                      </button>
                      <button
                        type="button"
                        onClick={() => {
                          console.log(
                            `${new Date()}: Pretending we're removing user ${
                              model.userFocus?.user?.id_str
                            } (${
                              model.userFocus?.user?.screen_name
                            }) from list ${l.id_str} (${l.slug})`,
                          );
                        }}
                      >
                        {'-'}
                      </button>
                    </div>
                  </li>
                ))}
              </ul>
            );
          case 'error':
            return (
              <div className="select">
                An error occurred.
                <code>
                  {JSON.stringify(model.allLists.error, undefined, 4)}
                </code>
              </div>
            );
          default:
            return typecheckNever(model.allLists);
        }
      })()}
    </>
  );
};

export const ListManagement: FC<Props> = (props) => {
  const effects = new Effects(props.dispatch);
  React.useEffect(() => {
    effects.loadListsFromIdb(
      /**
       * This whole callback pattern is such a hack.
       *
       * The problem here is that the `dispatch` function is (rightfully)
       * not in-scope inside the `reduce` function definition, and that
       * `Effects#loadListsFromIdb` returns `Msg` instead of some kind of
       * `Promise<twitter.List[]>`, so we can't just call `.then` here.
       *
       * It's likely that a better solution would involve some kind of
       * real effect-management library, like redux-loop or redux-saga.
       */
      (lists) => {
        // DEBUG: make debugging easier, fewer clicks
        if (lists) {
          const focal: twitter.List | undefined = lists[0];
          if (focal) {
            props.dispatch({
              type: 'focus_list',
              focusIdStr: focal.id_str,
            });
            props.dispatch(effects.loadListMembersFromIdb(focal.id_str));
          }
        }
      },
    );
  }, []);
  return (
    <div className="page list-management">
      <p>Manage your Twitter lists</p>
      <div className="sidescroll">
        <div className="source-list">
          <ListPicker {...props} />
          <ListMembersView
            focusedList={props.model.focusedList}
            userFocus={props.model.userFocus}
            dispatch={props.dispatch}
          />
        </div>
        <div className="focused-account">
          <FocusedUser
            userFocus={props.model.userFocus}
            dispatch={props.dispatch}
          />
        </div>
        <div className="destination-list">
          <DestinationListList {...props} />
        </div>
      </div>
    </div>
  );
};
