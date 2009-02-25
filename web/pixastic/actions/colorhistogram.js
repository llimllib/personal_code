/*
 * Pixastic Lib - Histogram - v0.1.0
 * Copyright (c) 2008 Jacob Seidelin, jseidelin@nihilogic.dk, http://blog.nihilogic.dk/
 * MIT License [http://www.opensource.org/licenses/mit-license.php]
 */

Pixastic.Actions.colorhistogram = {
  //TODO: we should probably make an object out of the data from prepareData and put this
  //      function in there and eliminate the data param. Have a version that does without
  //      the w and h too.
  each_pixel : function(data, w, h, visitor) {
    var w4 = w*4;
    var y = h;
    do {
      var offsetY = (y-1)*w4;
      var x = w;
      do {
        var offset = offsetY + (x*4-4);
        visitor(data[offset], data[offset+1], data[offset+2], data[offset+3]);
      } while (--x);
    } while (--y);
  },

  array256 : function(default_value) {
    arr = [];
    for (var i=0; i<256; i++) { arr[i] = default_value; }
    return arr
  },

  process : function(params) {
    var values = [];
    if (typeof params.options.returnValue != "object") {
      params.options.returnValue = {rvals:[], gvals:[], bvals:[]};
    }
    var returnValue = params.options.returnValue;
    if (typeof returnValue.values != "array") {
      returnValue.rvals = [];
      returnValue.gvals = [];
      returnValue.bvals = [];
    }

    if (Pixastic.Client.hasCanvasImageData()) {
      var data = Pixastic.prepareData(params);
      params.useData = false;

      var rvals = this.array256(0);
      var gvals = this.array256(0);
      var bvals = this.array256(0);

      var rect = params.options.rect;
      this.each_pixel(data, rect.width, rect.height, function(r, g, b, _) {
        rvals[r]++;
        gvals[g]++;
        bvals[b]++;
      });

      returnValue.rvals = rvals;
      returnValue.gvals = gvals;
      returnValue.bvals = bvals;

      return true;
    }
  },
  checkSupport : function() {
    return Pixastic.Client.hasCanvasImageData();
  }
}
