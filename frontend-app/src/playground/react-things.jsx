
import React from 'react';
import ReactDOM from 'react-dom';

/**
 * Entire module adapted from:
 *
 * - https://gist.github.com/gaearon/6668a1f6986742109c00a581ce704605
 *
 * via:
 *
 * - https://reactjs.org/docs/add-react-to-a-website.html#add-react-in-one-minute
 */

class LikeButton extends React.Component {
  /**
   * @param props {Object}
   */
  constructor(props) {
    super(props);
    this.state = { liked: false };
  }

  render() {
    if (this.state.liked) {
      return 'You liked this.';
    }

    return (
      <button onClick={ () => this.setState({ liked: true }) }>
          Like
      </button>
    );
  }
}

/**
 * @param elem {HTMLElement}
 */
export function loadReactComponentOnto(elem) {
  // Unmount: https://stackoverflow.com/a/44900331
  ReactDOM.unmountComponentAtNode(elem);
  ReactDOM.render(<LikeButton />, elem);
}
