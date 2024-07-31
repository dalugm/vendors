// ==UserScript==
// @name         Pixiv Enhance
// @namespace    someone
// @version      0.2.0
// @description  Enhance for pixiv
// @match        https://www.pixiv.net/novel/show.php?id=*
// @icon         https://www.pixiv.net/favicon.ico
// @run-at       document-idle
// @grant        GM_setClipboard
// ==/UserScript==

(() => {
  "use strict";

  new MutationObserver(check).observe(document, {
    childList: true,
    subtree: true,
  });

  function check(_changes, observer) {
    if (document.querySelector("h1")) {
      observer.disconnect();

      function createButton(text, fn) {
        const button = document.createElement("button");
        button.style = "margin-left: 4px; padding: 2px; font-size: 12px;";
        button.innerText = text;
        button.onclick = fn;
        return button;
      }

      function copyContentToClipboard() {
        const content = document.getElementById(
          "gtm-novel-work-scroll-begin-reading",
        ).parentNode.innerText;
        GM_setClipboard(content);
      }

      const title = document.querySelector("h1");

      const copyButton = createButton("复制全文", () =>
        copyContentToClipboard(),
      );
      title.append(copyButton);
    }
  }
})();
