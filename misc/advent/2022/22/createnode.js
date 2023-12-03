// https://github.com/jonathantneal/dom-create-node/blob/b64f3663f7076b3eea0f6268dcfb2aa0fda77ca6/index.es6
const MATCH = "[_a-zA-Z]+[_a-zA-Z0-9-]*";
const QUOTE = "([\"'])((?:(?=(\\\\?))\\7.)*?)\\5";
const QUERY =
  "^(" +
  MATCH +
  ")|^#(-?" +
  MATCH +
  ")|^\\.(-?" +
  MATCH +
  ")|^\\[(-?" +
  MATCH +
  ")(?:=" +
  QUOTE +
  ")?\\]|^" +
  QUOTE.replace(5, 8).replace(7, 10) +
  "|^\\s*([<+-]+)\\s*|^\\s*(\\s+|>)\\s*";

export default function createNode(selector) {
  let reducable = selector;
  let root = document.createElement("div");
  let node = root;
  let match;

  while ((match = reducable && reducable.match(QUERY))) {
    // element
    if (match[1]) {
      let temp = document.createElement(match[1]);

      if (node.parentNode) {
        node.parentNode.replaceChild(temp, node);

        node = temp;
      } else if (root === node) {
        root = node = temp;
      } else {
        node = node.appendChild(temp);
      }
    }

    // id
    if (match[2]) {
      node.id = match[2];
    }

    // class
    if (match[3]) {
      node.classList.add(match[3]);
    }

    // attribute
    if (match[4]) {
      node.setAttribute(match[4], match[6] || "");
    }

    // text
    if (match[8]) {
      let temp = document.createTextNode(match[9]);

      if (node.parentNode) {
        node.parentNode.replaceChild(temp, node);

        node = temp;
      } else if (root === node) {
        root = node = temp;
      } else {
        node = node.appendChild(temp);
      }

      node = temp;
    }

    // traversing
    if (match[11]) {
      let index = -1;
      let char;

      while ((char = match[11][++index])) {
        if (char === "<") {
          node = node.parentNode;
        } else if (char === "+") {
          node = node.nextElementSibling || node;
        } else if (char === "-") {
          node = node.previousElementSibling || node;
        }
      }

      node = node.appendChild(document.createElement("div"));
    }

    // nesting
    if (match[12]) {
      node = node.appendChild(document.createElement("div"));
    }

    reducable = reducable.slice(match[0].length);
  }

  return root;
}
