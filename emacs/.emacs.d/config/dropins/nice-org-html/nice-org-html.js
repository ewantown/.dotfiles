/*
;; nice-org-html.js
;;==============================================================================
;; Copyright (C) 2024, Ewan Townshend

;; Author: Ewan Townshend
;; URL: https://github.com/ewantown/nice-org-html
;; Version: 1.2

;;==============================================================================
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; This file is not part of emacs.

;;==============================================================================
;; JS for html exported by et-org-html.el
 */

const preamble = document.getElementById("preamble");
const controls = document.getElementById("view-controls");
const toggleTocBtn = document.getElementById("toggle-toc");
const gotoTopBtn = document.getElementById("goto-top");
const toggleModeBtn = document.getElementById("toggle-mode");
const content = document.getElementById("content");
const toc = document.getElementById("table-of-contents");

// Move sticky control bar (injected within preamble)
document.body.insertBefore(controls, content);

// Mode handling
function getMode() {
  let cookie = document.cookie.split('; ').find(r => r.startsWith('mode'))
  return cookie && cookie.split("=")[1];
}

function setMode(mode) {
  document.body.dataset.mode = mode;
  let thmCookie = document.cookie.split('; ').find(r => r.startsWith(mode));
  document.body.dataset.theme = thmCookie ? thmCookie.split('=')[1] : 'unknown';
  toggleModeBtn.innerHTML = (mode === 'light') ? '&#9789;' : '&#9788;';
  document.cookie = 'mode=' + mode;
}

// Initially, set mode based on stored cookie
let mode = getMode();
if (mode && ["light", "dark"].includes(mode)) {
  setMode(mode);
} else {
  let prop = "prefers-color-scheme";
  let pref = (mode === "query") && window.matchMedia &&
    ["light", "dark"].find(s => window.matchMedia(`(${prop}: ${s})`).matches);
  setMode(pref || "dark"); // fallback: dark
}

// Mode toggling
toggleModeBtn.addEventListener('click', () => {
  setMode(document.body.dataset.mode === 'dark' ? 'light' : 'dark');
})


// Jump to top
let scrollY = document.documentElement.scrollTop;
window.addEventListener('scroll', () => {
  let atTop = document.documentElement.scrollTop <= preamble.offsetHeight;
  gotoTopBtn.dataset.show = !atTop + "";
});
gotoTopBtn.addEventListener('click', () => {
  window.scrollTo({
    top: 0,
    behavior: "smooth"
  });
})

// Table-of-contents toggling
if (toc) {
  toggleTocBtn.dataset.show = 'true';
  toggleTocBtn.addEventListener('click', () => {
    let showingToc = document.body.dataset.toc;
    scrollY = showingToc ? scrollY : document.documentElement.scrollTop;
    if (showingToc) {
      document.body.dataset.toc = '';
      document.documentElement.scrollTop = scrollY;
    } else {
      let tocHeight =
	Math.max(
	  window.innerHeight - controls.offsetHeight,
	  toc.offsetHeight
	);
      const header = document.getElementById("injected-header");
      if (header && (scrollY > header.offsetHeight)) {
	document.documentElement.scrollTop = preamble.offsetHeight;
      }
      toc.style.height = `${tocHeight}px`;
      document.body.dataset.toc = 'true';
    }
    toc.addEventListener('click', () => {
      if (document.body.dataset.toc) {
	document.body.dataset.toc = '';
	document.documentElement.scrollTop = scrollY;
      }
    })
  })
};

// Instrument anchor linking for sticky control bar
document.querySelectorAll('a[href^="#"]').forEach(anchor => {
  anchor.addEventListener('click', (e) => {
    e.preventDefault();
    const targetId = e.currentTarget.getAttribute('href');
    const target = document.querySelector(targetId);

    setTimeout(() => {
      const targetPos = target.getBoundingClientRect().top;
      let offset = targetPos + document.documentElement.scrollTop;
      offset -= controls.offsetHeight;
      window.scrollTo({
	top: offset,
	behavior: "smooth"
      });
    }, 0);
  });
});

function copyTextToClipboard(text) {
  // Make invisible textarea
  var textArea = document.createElement("textarea");
  textArea.style.position = 'fixed';
  textArea.style.top = 0;
  textArea.style.left = 0;
  textArea.style.width = '2em';
  textArea.style.height = '2em';
  textArea.style.padding = 0;
  textArea.style.border = 'none';
  textArea.style.outline = 'none';
  textArea.style.boxShadow = 'none';
  textArea.style.background = 'transparent';

  // Copy contents into it
  textArea.value = text;

  // Try to copy from it to clipboard
  document.body.appendChild(textArea);
  textArea.focus();
  textArea.select();
  var res;
  try {
    res = document.execCommand('copy');
  } catch (err) {
    res = false;
  }

  // Remove invisible textarea
  document.body.removeChild(textArea);

  return res;
}

