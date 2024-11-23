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
  toc.dataset.show = '';
  toggleTocBtn.dataset.show = 'true';
  toggleTocBtn.addEventListener('click', function() {
    toc.dataset.show = toc.dataset.show ? '' : 'true';
    document.body.dataset.toc = toc.dataset.show;
    toc.addEventListener('click', function() {
      if (toc.dataset.show) {
	toc.dataset.show = "";
      }
      if (document.body.dataset.toc) {
	document.body.dataset.toc = "";
      }
    })
  })
} else {
  document.body.dataset.toc = 'false';
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
