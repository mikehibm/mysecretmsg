/* ピッチインピッチアウトによる拡大縮小を禁止 */
document.documentElement.addEventListener(
  'touchstart',
  function(e) {
    if (e.touches.length >= 2) {
      e.preventDefault();
    }
  },
  { passive: false }
);
/* ダブルタップによる拡大を禁止 */
var t = 0;
document.documentElement.addEventListener(
  'touchend',
  function(e) {
    var now = new Date().getTime();
    if (now - t < 550) {
      e.preventDefault();
    }
    t = now;
  },
  false
);
