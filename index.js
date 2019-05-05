import { Elm } from './src/Main.elm';

Elm.Main.init({
  node: document.getElementById('elm'),
});

// Avoid focus to remain on any button.
const input = document
  .querySelectorAll('button')
  .forEach((btn) => btn.addEventListener('click', (e) => e.target.blur()));
