Pixastic.Actions.weighted_desaturate = {
  each_pixel : function(data, w, h, visit) {
    var w4 = w*4;
    var y = h;
    do {
      var offsetY = (y-1)*w4;
      var x = w;
      do {
        var offset = offsetY + (x-1)*4;
        visit(offset, data[offset], data[offset+1], data[offset+2], data[offset+3]);
      } while (--x);
    } while (--y);
  }

	process : function(params) {
    var rweight = params.options.r;
    var gweight = params.options.g;
    var bweight = params.options.b;

    var scale = 100 / (r + g + b);
    rweight *= scale;
    gweight *= scale;
    bweight *= scale;

		if (Pixastic.Client.hasCanvasImageData()) {
			var data = Pixastic.prepareData(params);
			var rect = params.options.rect;
			var w = rect.width;
			var h = rect.height;
      each_pixel(data, w, h, function(i, r, g, b, a) {
        brightness = r * rweight + g * gweight + b * bweight;
        data[i] = data[i+1] = data[i+2] = brightness;
      }
			return true;
    }
  }
	checkSupport : function() {
		return Pixastic.Client.hasCanvasImageData();
	}
}
