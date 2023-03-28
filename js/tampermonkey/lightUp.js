// ==UserScript==
// @name               点亮网页
// @namespace          any
// @version            0.1.0
// @description        点亮网页，那么灰谁看得下去？
// @author             you
// @match              http://*/*
// @match              https://*/*
// @run-at             document-start
// @grant              none
// @license            wtfpl
// ==/UserScript==

const css = document.createElement("style");

css.innerHTML = `
html, body, *, .fullgray, header-gray, .gray {
  -webkit-filter: grayscale(0) !important;
  -moz-filter: grayscale(0) !important;
  -ms-filter: grayscale(0) !important;
  -o-filter: grayscale(0) !important;
  filter: grayscale(0) !important;
  filter: progid:DXImageTransform.Microsoft.BasicImage(grayscale=0) !important;
  filter: none !important;
}`;

document.documentElement.append(css);
