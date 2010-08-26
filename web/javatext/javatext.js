if (!window.CanvasRenderingContext2D)
    window.CanvasRenderingContext2D = document.createElement("canvas").getContext("2d").__proto__;
else
    window.CanvasRenderingContext2D = CanvasRenderingContext2D.prototype

if (CanvasRenderingContext2D.fillText == undefined) {
    CanvasRenderingContext2D.fillText = function(text, x, y, maxWidth) {
        this.translate(x, y);
        s = "" + document.texter.renderString(text);
        eval(s);
        this.fill();
        this.translate(-x, -y);
    }
}

if (CanvasRenderingContext2D.strokeText == undefined) {
    CanvasRenderingContext2D.strokeText = function(text, x, y, maxWidth) {
        this.translate(x, y);
        s = "" + document.texter.renderString(text);
        eval(s);
        this.stroke();
        this.translate(-x, -y);
    }
}

if (CanvasRenderingContext2D.font == undefined) {
    CanvasRenderingContext2D._font = "sans-serif";

    CanvasRenderingContext2D.__defineGetter__("font", function() {
            return this._font;
    });
    CanvasRenderingContext2D.__defineSetter__("font", function(val) {
        var fnt = val.match(/\s*(\d+)\s*px\s*(\w+)/);
        var sz = fnt[1];
        var face = fnt[2];

        this._font = face;
        document.texter.setFont(sz, face);
    });
}
