// ==UserScript==
// @name         Pixiv Enhance
// @namespace    someone
// @version      0.1.1
// @description  Enhance for pixiv
// @match        https://www.pixiv.net/novel/show.php?id=*
// @icon         https://www.google.com/s2/favicons?domain=pixiv.net
// @grant        GM_setClipboard
// ==/UserScript==

const getJQuery = async () => {
  return fetch("https://unpkg.com/jquery")
    .then((response) => response.text())
    .then((script) => {
      const scriptEl = document.createElement("script");
      scriptEl.textContent = script;
      document.head.appendChild(scriptEl);
      return $;
    });
};

const addButton = (text, message) => {
  $("<button>")
    .text(message)
    .appendTo("h1")
    .css({
      "margin-left": "10px",
      "font-size": "12px",
      "padding": "4px 8px",
    })
    .on("click", () => {
      copyToClipboard(text);
    });
};

const processText = ($) => {
  $("<br />")
    .appendTo("h1")
    .css("margin", "4px 0");

  const parent = $("#gtm-novel-work-scroll-begin-reading").parent();
  // const parent = $(".text-count").parent();
  if (!!parent.length) {
    const text = parent[0].innerText;
    addButton(text, "复制全文（保留换行符）");
  }

  const text = $("#gtm-novel-work-scroll-begin-reading").parent().text();
  // const text = $(".text-count").parent().text();
  if (!!text) {
    addButton(text, "复制全文（纯文本）");
  }
};

const useJQuery = async () => {
  const $ = await getJQuery();
  processText($);
};

const copyToClipboard = (text) => GM_setClipboard(text);

(() => {
  "use strict";

  useJQuery();
})();
