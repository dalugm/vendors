// ==UserScript==
// @name         Rule34 Downloader
// @namespace    someone
// @version      0.1.0
// @description  Easier download from rule34.xxx
// @match        https://rule34.xxx/*
// @icon         https://www.google.com/s2/favicons?domain=rule34.xxx
// @connect      https://rule34.xxx/*
// @grant        GM_download
// ==/UserScript==

const curry = (fn) => {
  const arity = fn.length;
  return function curried(...args) {
    if (args.length >= arity) {
      return fn.apply(this, args);
    } else {
      return (...nextArgs) => {
        return curried.apply(this, args.concat(nextArgs));
      };
    }
  };
};

const createButton = curry((text, fn) => {
  const button = document.createElement("button");
  button.innerHTML = `<p style='margin: 2px; font-size: 12px;'> ${text} </p>`;
  button.onclick = fn;
  return button;
});

const getOriginalImageUrlFromSideBarList = curry((optionText, lists) => {
  const optionLinkHTML = Array.from(lists).find(
    (list) => list.textContent.indexOf(optionText) !== -1,
  );
  const origLink = optionLinkHTML.querySelector("a[href*='image']");
  return origLink ? origLink.href : null;
});

const getFilenameFromLink = (link) => {
  const result = link.slice(link.lastIndexOf("/") + 1, link.lastIndexOf("?"));
  return result;
};

const extractImageUrl = (html) => {
  const tmp = document.createElement("div");
  tmp.innerHTML = html;

  const list = tmp.querySelectorAll("div.link-list");
  const getOriginalImageUrl = getOriginalImageUrlFromSideBarList("Options");
  return getOriginalImageUrl(list);
};

const downloadImage = (imagePageLink) => {
  fetch(imagePageLink)
    .then((response) => response.text())
    .then((html) => onDownload(extractImageUrl(html)))
    .catch((error) => console.error(error));
};

const viewImage = (imagePageLink) => {
  fetch(imagePageLink)
    .then((response) => response.text())
    .then((html) => window.open(extractImageUrl(html)))
    .catch((error) => console.error(error));
};

const addDownloadLinkOnViewPage = () => {
  const imageSublinks = document.querySelector("h4.image-sublinks");
  const downloadLink = document.createElement("button");
  downloadLink.innerHTML = "Download";

  const linkLists = document.querySelectorAll("div.link-list");
  const getOriginalImageUrl = getOriginalImageUrlFromSideBarList("Options");
  const link = getOriginalImageUrl(linkLists);

  downloadLink.onclick = () => {
    onDownload(link);
  };
  imageSublinks.append(downloadLink);
};

const onDownload = (link) => {
  console.log(new Date().toLocaleString(), link);
  const filename = getFilenameFromLink(link);
  GM_download({ url: link, name: filename });
};

(() => {
  "use strict";

  const contentDiv = document.querySelector("div.content div");
  const posts = contentDiv.querySelectorAll("span");

  if (document.location.href.indexOf("s=view") !== -1) {
    addDownloadLinkOnViewPage();
    // Skip remaining steps if on the view page.
    return;
  }

  // Add buttons for each image.
  posts.forEach((post) => {
    const link = post.querySelector("a").href;

    const downloadButton = createButton("Download", () => downloadImage(link));
    post.insertBefore(downloadButton, post.children[0]);
    const viewButton = createButton("View", () => viewImage(link));
    post.insertBefore(viewButton, downloadButton);

    post.insertBefore(document.createTextNode(" | "), downloadButton);
  });

  // Download all images on the page.
  const downloadAllImages = () => {
    posts.forEach((post) => {
      const link = post.querySelector("a").href;
      downloadImage(link);
    });
  };

  const buttonAll = document.createElement("button");
  buttonAll.style = "width: 100%";
  buttonAll.innerHTML =
    "<p style='margin: 2px; font-size: 16px;'> Download All </p>";
  buttonAll.onclick = downloadAllImages;
  document.querySelector("div.content").prepend(buttonAll);
})();
