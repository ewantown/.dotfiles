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
