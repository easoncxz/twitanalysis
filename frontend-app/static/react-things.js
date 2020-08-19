
import {
  React,
  ReactDOM,
} from "./dependencies.js";

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
  constructor(props) {
    super(props);
    this.state = { liked: false };
  }

  render() {
    if (this.state.liked) {
      return "You liked this.";
    }

    return React.createElement(
      "button",
      { onClick: () => this.setState({ liked: true }) },
      "Like"
    );
  }
}

export function loadReactComponentOnto(elem) {
  // Unmount: https://stackoverflow.com/a/44900331
  ReactDOM.unmountComponentAtNode(elem);
  ReactDOM.render(React.createElement(LikeButton), elem);
}
