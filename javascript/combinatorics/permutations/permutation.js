/**
 * Swaps two values in an array.
 * @param {Array} items The array containing the items.
 * @param {int} firstIndex Index of first item to swap.
 * @param {int} secondIndex Index of second item to swap.
 * @return {void}
 */
function swap(items, firstIndex, secondIndex){
    var temp = items[firstIndex];
    items[firstIndex] = items[secondIndex];
    items[secondIndex] = temp;
}

//assumes array @arr is sorted
//implementation of Knuth's algorithm L, page 1, fascicle 2b, TAOCP
function permute(arr, visit) {
  visit(arr.slice(0));

  if (arr.length == 1) { return; }

  var n = arr.length -1;
  while (1) {
    var j = n - 1;
    while (arr[j] >= arr[j+1]) {
      j -= 1;
      if (j == -1) { return; } //terminate
    }
    l = n;
    while (arr[j] >= arr[l]) {
      l -= 1;
    }
    swap(arr, j, l);
    k = j+1;
    l = n;
    while (k < l) {
      swap(arr, k, l);
      k += 1;
      l -= 1;
    }
    visit(arr.slice(0));
  }

}
