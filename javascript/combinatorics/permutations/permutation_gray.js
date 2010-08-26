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

//assumes array @arr is sorted and has no duplicate elements
//implementation of Knuth's algorithm P, page 4, fascicle 2b, TAOCP
//permutes arr via Gray codes, with only one interchange between any two
//permutations
function permute_gray(arr, visit) {
  visit(arr.slice(0));

  if (arr.length == 1) { return; }

  var n = arr.length - 1;

  //initialize c and o to arrays of 0 and 1 of length n+1
  var c = Array(n+1);
  var o = Array(n+1);
  for (var i in arr) {
    c[i] = 0;
    o[i] = 1;
  }
  c[n] = 0;
  o[n] = 1;

  j = n;
  s = 0;

  while (1) {
    q = c[j] + o[j];
    console.log("c[j], o[j]", c[j], o[j]);
    if (q >= 0 && q != j+1) {
      swap(arr, j-c[j]+s, j-q+s);
      visit(arr.slice(0));
      c[j] = q;
      j = n;
      s = 0;
      continue;
    }
    else {
      if (j == 1) { return; } //terminate
      s += 1;
    }
    o[j] = -o[j];
    j -= 1;
  }
}
