"use strict";

import React from "react";
import ReactDOM from "react-dom";

console.log("main.js here. Nothing to show.");

const e = React.createElement;

function App() {
  return e("p", {}, "Hello from React");
}

const mountPoint = document.getElementById("react-mountpoint");
const renderRoot = () => {
  ReactDOM.render(<App />, mountPoint);
};

if (typeof window === "object" && window.addEventListener) {
  window.addEventListener("load", () => {
    renderRoot();
  });
}
