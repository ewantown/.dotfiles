let theme = 'dark';  // default

let theme_cookie = document.cookie.split('; ').find(r => r.startsWith('org_html_themify_theme'))
if (theme_cookie) {
  theme = theme_cookie.split('=')[1]
}

let toggleThemeBtn = document.getElementById('toggle-theme')

if (theme == 'light') {
  useLightTheme();
} else {
  useDarkTheme();
}

function transition() {
  document.body.classList.add('theme-transition')
  setTimeout(() => document.body.classList.remove('theme-transition'), 500)
}

function useLightTheme(theme) {
  document.body.dataset.theme = 'light'
  toggleThemeBtn.innerHTML = '&#9789;'
  document.cookie = 'org_html_themify_theme=light'
}

function useDarkTheme(theme) {
  document.body.dataset.theme = 'dark'
  toggleThemeBtn.innerHTML = '&#9788;'
  document.cookie = 'org_html_themify_theme=dark'
}

toggleThemeBtn.addEventListener('click', function() {
  transition()
  if (document.body.dataset.theme == 'light') {
    useDarkTheme();
  } else {
    useLightTheme();
  }
})


let toggleTocBtn = document.getElementById('toggle-toc')
let toc = document.getElementById('table-of-contents')

if (toc) {
  document.body.dataset.toc = 'true';
  toc.classList.add('toc-show')
  toggleTocBtn.addEventListener('click', function() {
    toc.classList.add('toc-show')
    toc.addEventListener('click', function() {
      toc.classList.remove('toc-show')
    })
  })
} else {
  document.body.dataset.toc = 'false';
}

function copyTextToClipboard(text) {
  var textArea = document.createElement("textarea");

  // Place in the top-left corner of screen regardless of scroll position.
  textArea.style.position = 'fixed';
  textArea.style.top = 0;
  textArea.style.left = 0;

  // Ensure it has a small width and height. Setting to 1px / 1em
  // doesn't work as this gives a negative w/h on some browsers.
  textArea.style.width = '2em';
  textArea.style.height = '2em';

  // We don't need padding, reducing the size if it does flash render.
  textArea.style.padding = 0;

  // Clean up any borders.
  textArea.style.border = 'none';
  textArea.style.outline = 'none';
  textArea.style.boxShadow = 'none';

  // Avoid flash of the white box if rendered for any reason.
  textArea.style.background = 'transparent';

  textArea.value = text;

  document.body.appendChild(textArea);
  textArea.focus();
  textArea.select();

  try {
    var successful = document.execCommand('copy');
    var msg = successful ? 'successful' : 'unsuccessful';
    console.log('Copying text command was ' + msg);
  } catch (err) {
    console.log('Unable to copy text');
  }

  document.body.removeChild(textArea);
}
