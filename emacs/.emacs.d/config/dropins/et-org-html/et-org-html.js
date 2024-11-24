/*
;; et-org-html.js
;;==============================================================================
;; Copyright (C) 2024, Ewan Townshend

;; Author: Ewan Townshend
;; URL: TBD
;; Version: 1.0

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

let cookie = document.cookie.split('; ').find(r => r.startsWith('theme-mode'))

let mode = cookie ? cookie.split('=')[1] : 'dark'; // Default

let toggleModeBtn = document.getElementById('toggle-mode');

setMode(mode);

function setMode(mode) {
  document.body.dataset.mode = mode;
  toggleModeBtn.innerHTML = (mode === 'light') ? '&#9789;' : '&#9788;';
  document.cookie = 'theme-mode=' + mode;
}

toggleModeBtn.addEventListener('click', () => {
  mode = document.body.dataset.mode === 'dark' ? 'light' : 'dark';
  setMode(mode);
})


let toggleTocBtn = document.getElementById('toggle-toc')
let toc = document.getElementById('table-of-contents')

if (toc) {
  toggleTocBtn.dataset.show = 'true';
  toggleTocBtn.addEventListener('click', () => {
    let showingToc = document.body.dataset.toc;
    document.body.dataset.toc = showingToc ? '' : 'true';
    toc.addEventListener('click', () => {
      if (document.body.dataset.toc) {
	document.body.dataset.toc = "";
      }
    })
  })
}

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
  try {
    var success = document.execCommand('copy');
    console.log(success ? 'Copied text' : 'Could not copy text');
  } catch (err) {
    console.log('Error copying text');
  }

  // Remove invisible textarea
  document.body.removeChild(textArea);
}
