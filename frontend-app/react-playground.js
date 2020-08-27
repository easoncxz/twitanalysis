
//import React from 'react';
//import ReactDOM from 'react-dom';
//import Redux from 'redux';
//import ReduxThunk from 'redux-thunk';
const thunk = ReduxThunk.default;

const init = {
  cancelHandle: undefined,
  now: new Date(),
};

const update = (current, action) => {
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
  default:
    return current;
  }
};

const store = Redux.createStore(
  update,
  init,
  Redux.applyMiddleware(thunk),
);

const effectsOf = dispatch => ({
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
});

class Clock extends React.Component {
  constructor(props) {
    super(props);
    this.props = props;
  }

  render() {
    return (
      <div>
        <h1>The time is currently:</h1>
        <p><code>{ this.props.model.now.toISOString() }</code></p>
        <p>
          (The clock is{
            this.props.model.cancelHandle === undefined
              ? ' not'
              : ''
          } ticking.)
        </p>
      </div>
    );
  }
}

const mountPoint = document.getElementById('root');

const renderRoot = () => {
  ReactDOM.render(
    <Clock
      model={store.getState()}
    />,
    mountPoint
  );
};

store.subscribe(renderRoot);

const effects = effectsOf(store.dispatch.bind(store));

store.dispatch(effects.startClock());

//setInterval(
//  () => { store.dispatch(effects.tick(new Date())); },
//  1000,
//);
