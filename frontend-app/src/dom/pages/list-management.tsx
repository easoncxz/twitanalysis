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
  queryParams,
} from '../utils/utils';

type MyDispatch<T> = (_: T) => T;

type ListAndMembers = {
  list: twitter.List;
  members: RemoteData<twitter.User[], Error>;
};

export type Model = {
  allLists: RemoteData<twitter.List[], Error>;
  focusedList: MaybeDefined<ListAndMembers>;
};

export const init: Model = {
  allLists: { type: 'idle' },
  focusedList: undefined,
};

export type Msg =
  | { type: 'noop' }
  | {
      type: 'focus_list';
      focusIdStr: string;
    }
  | {
      type: 'fetch_lists';
      update: RemoteData<twitter.List[], Error>;
    }
  | {
      type: 'fetch_list_members';
      listIdStr: string;
      members: RemoteData<twitter.User[], Error>;
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
    case 'fetch_list_members': {
      if (model.focusedList === undefined) {
        // No focused list; ignoring any incoming results
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
    default:
      typecheckNever(msg);
      return model;
  }
};

export class Effects {
  constructor(private readonly dispatch: MyDispatch<Msg>) {}

  loadListsFromIdb<T>(cont?: (_: twitter.List[]) => T): Msg {
    tdb.readLists().then(
      (lists: twitter.List[]) => {
        const sorted = lists
          .slice()
          .sort((l, r) => (l.name < r.name ? -1 : l.name > r.name ? 1 : 0));
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
            t('lists/members') + '?' + queryParams([['list_id', listIdStr]]),
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
  focus: MaybeDefined<ListAndMembers>;
  dispatch: MyDispatch<Msg>;
}> = ({ focus, dispatch }) => {
  const effects = new Effects(dispatch);
  if (focus === undefined) {
    return <p>No list in focus</p>;
  } else {
    return (
      <>
        <div>
          <button
            type="button"
            onClick={() =>
              dispatch(effects.fetchListMembersFromNetwork(focus.list.id_str))
            }
          >
            fetch
          </button>
          <button
            type="button"
            onClick={() => {
              dispatch(
                effects.saveListMembershipsToIdb(
                  focus.list.id_str,
                  focus.members,
                ),
              );
            }}
          >
            save
          </button>
          <button
            type="button"
            onClick={() => {
              dispatch(effects.loadListMembersFromIdb(focus.list.id_str));
            }}
          >
            load
          </button>
        </div>
        {(() => {
          switch (focus.members.type) {
            case 'idle':
              return <p>Members not yet known.</p>;
            case 'loading':
              return <p>Loading list members...</p>;
            case 'ok': {
              const users: twitter.User[] = focus.members.data;
              return (
                <ul>
                  {users.map((u) => (
                    <li className="source-list-user" key={u.id_str}>
                      {u.screen_name}
                    </li>
                  ))}
                </ul>
              );
            }
            case 'error':
              return <p>Error fetching list members.</p>;
            default:
              typecheckNever(focus.members);
              return focus;
          }
        })()}
      </>
    );
  }
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
            focus={props.model.focusedList}
            dispatch={props.dispatch}
          />
        </div>
        <div className="focused-account">(Focused account)</div>
        <div className="destination-list">
          Destination list:
          <ul>
            <li>(foo list)</li>
            <li>(bar list)</li>
          </ul>
        </div>
      </div>
    </div>
  );
};
