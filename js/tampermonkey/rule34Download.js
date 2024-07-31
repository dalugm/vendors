// ==UserScript==
// @name         Rule34 Downloader
// @namespace    someone
// @version      0.1.1
// @description  Easier download from rule34.xxx
// @match        https://rule34.xxx/*
// @icon         https://rule34.xxx/favicon.ico?v=2
// @connect      https://rule34.xxx/*
// @grant        GM_download
// ==/UserScript==

(() => {
  "use strict";

  function createButton(text, fn) {
    const button = document.createElement("button");
    button.style = "margin: 2px; font-size: 12px;";
    button.innerText = text;
    button.onclick = fn;
    return button;
  }

  function getOriginalImageUrlFromSideBarList(optionText, lists) {
    const optionLinkHTML = Array.from(lists).find((list) => {
      return list.textContent.includes(optionText);
    });
    const origLink = optionLinkHTML.querySelector("a[href*='image']");
    return origLink ? origLink.href : null;
  }

  function getFilenameFromLink(link) {
    const result = link.slice(link.lastIndexOf("/") + 1, link.lastIndexOf("?"));
    return result;
  }

  function extractImageUrl(html) {
    const tmp = document.createElement("div");
    tmp.innerHTML = html;

    const lists = tmp.querySelectorAll("div.link-list");
    return getOriginalImageUrlFromSideBarList("Options", lists);
  }

  function viewImage(imagePageLink) {
    fetch(imagePageLink)
      .then((response) => response.text())
      .then((html) => window.open(extractImageUrl(html)))
      .catch((error) => console.error(error));
  }

  function downloadImage(imagePageLink) {
    fetch(imagePageLink)
      .then((response) => response.text())
      .then((html) => onDownload(extractImageUrl(html)))
      .catch((error) => console.error(error));
  }

  // Download all images on the page.
  function downloadAllImages() {
    posts.forEach((post) => {
      const link = post.querySelector("a").href;
      downloadImage(link);
    });
  }

  function addDownloadLinkOnViewPage() {
    const imageSublinks = document.querySelector("h4.image-sublinks");
    const downloadLink = document.createElement("button");
    downloadLink.innerHTML = "Download";

    const linkLists = document.querySelectorAll("div.link-list");
    const originalImageUrl = getOriginalImageUrlFromSideBarList(
      "Options",
      linkLists,
    );

    downloadLink.onclick = () => {
      onDownload(originalImageUrl);
    };
    imageSublinks.append(downloadLink);
  }

  function onDownload(link) {
    console.log(new Date().toLocaleString(), link);
    const filename = getFilenameFromLink(link);
    GM_download({ url: link, name: filename });
  }

  const contentDiv = document.querySelector("div.content div");
  const posts = contentDiv.querySelectorAll("span");

  if (document.location.href.includes("s=view")) {
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

  const buttonAll = document.createElement("button");
  buttonAll.style = "width: 100%; font-size: 16px";
  buttonAll.innerText = "Download ALL";
  buttonAll.onclick = downloadAllImages;
  document.querySelector("div.content").prepend(buttonAll);
})();
