
// @ts-nocheck

/*globals React, ReactDOM, Redux */

//import React from 'react';
//import ReactDOM from 'react-dom';
//import Redux from 'redux';
//import ReduxThunk from 'redux-thunk';
//const thunk = ReduxThunk.default;

// This TypeScript documentation page is gold:
//
//    https://www.typescriptlang.org/docs/handbook/jsdoc-supported-types.html#typedef-callback-and-param

/**
 * @typedef {{
 *  cancelHandle: NodeJS.Timeout | undefined,
 *  now: Date,
 * }} Model
 */

/**
 * @type {Model}
 */
const init = {
  cancelHandle: undefined,
  now: new Date(),
};

/**
 * @type {Redux.Reducer<Model, Action>}
 */
const reducer = (current, action) => {
  if (!current) {
    return init;
  }
  switch (action.type) {
  case 'tick':
    return {
      ...current,
      now: action.timestamp,
    };
  case 'start_clock':
    return {
      ...current,
      cancelHandle: action.cancelHandle,
    };
  case 'stop_clock':
    return {
      ...current,
      cancelHandle: undefined,
    };
  default:
    return current;
  }
};

/**
 * @type {Redux.Store}
 */
const store = Redux.createStore(
  reducer,
  init,
  //Redux.applyMiddleware(thunk),
);

/**
 * @typedef {({
 *      type: 'tick',
 *      timestamp: Date,
 *    } | {
 *      type: 'start_clock',
 *      cancelHandle: NodeJS.Timeout,
 *    } | {
 *      type: 'stop_clock',
 *    }
 * )} Action
 *
 * @typedef {{
 *  tick(t: Date): Action,
 *  startClock(): Action,
 *  stopClock(n: NodeJS.Timeout): Action,
 * }} ActionCreators
 */

/**
 * @type {(_: Redux.Dispatch<Action>) => ActionCreators}
 */
const actionsOf = dispatch => ({
  tick(t) {
    return {
      type: 'tick',
      timestamp: t,
    };
  },
  startClock() {
    return {
      type: 'start_clock',
      cancelHandle: setInterval(
        () => { dispatch(this.tick(new Date())); },
        1000
      ),
    };
  },
  stopClock(handle) {
    clearInterval(handle);
    return {
      type: 'stop_clock',
    };
  },
});


class Clock extends React.Component {
  /**
   * @constructor
   * @param {{
   *    model: Model,
   *    dispatch: Redux.Dispatch<Action>,
   *    actions: ActionCreators,
   * }} props
   */
  constructor(props) {
    super(props);
    this.props = props;
  }

  renderClockDescription() {
    const props = this.props;
    const h = props.model.cancelHandle;
    if (h === undefined) {
      return (
        <div>
          <p>(The clock is stopped.)</p>
          <button onClick={
            () => props.dispatch(props.actions.startClock())
          }>Start</button>
        </div>
      );
    } else {
      return (
        <div>
          <p>(The clock is ticking.)</p>
          <button onClick={
            () => props.dispatch(props.actions.stopClock(h))
          }>Stop</button>
        </div>
      );
    }
  }

  render() {
    const styling =
      this.props.model.cancelHandle === undefined
        ? { className: 'red-frame' }
        : { className: 'green-frame' };
    return (
      <div>
        <h1>The time is currently:</h1>
        <p><code {...styling}>{ this.props.model.now.toISOString() }</code></p>
        {this.renderClockDescription()}
      </div>
    );
  }
}

const mountPoint = document.getElementById('root');
const renderRoot = () => {
  ReactDOM.render(
    <Clock
      model={store.getState()}
      dispatch={store.dispatch.bind(store)}
      actions={actionsOf(store.dispatch.bind(store))}
    />,
    mountPoint
  );
};
store.subscribe(renderRoot);

renderRoot();
