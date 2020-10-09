
/* global React, ReactDOM, Redux, ReduxThunk */

//import React from 'react';
//import ReactDOM from 'react-dom';
//import Redux from 'redux';
//import ReduxThunk from 'redux-thunk';
//const thunk = ReduxThunk.default;

const init = {
  cancelHandle: undefined,
  now: new Date(),
};

const reducer = (current, action) => {
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
      cancelHandle: action.cancelHandle,
    };
  default:
    return current;
  }
};

const store = Redux.createStore(
  reducer,
  init,
  //Redux.applyMiddleware(thunk),
);

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
    return {
      type: 'stop_clock',
      cancelHandle: void clearInterval(handle),
    };
  },
});


class Clock extends React.Component {
  constructor(props) {
    super(props);
    this.props = props;
  }

  renderClockDescription() {
    const props = this.props;
    if (props.model.cancelHandle === undefined) {
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
            () => props.dispatch(props.actions.stopClock(props.model.cancelHandle))
          }>Stop</button>
        </div>
      );
    }
  }

  render() {
    const styling =
      this.props.model.cancelHandle === undefined
        ? { class: 'red-frame' }
        : { class: 'green-frame' };
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
