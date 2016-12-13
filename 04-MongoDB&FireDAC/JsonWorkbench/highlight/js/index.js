
// Helper to load the syntax highlight code
var BoxCode = function (el) {
  this.el = el;
  this.wrap = false;
  this.code = "";
  this.language = "";
  this.resize();
  
  // attach the resize event
  var _this = this;
  window.onresize = function() {
     _this.resize();
  }
}

BoxCode.prototype = {

  resize: function() {
    var w = window,
        d = document,
        e = d.documentElement,
        g = d.getElementsByTagName('body')[0],
        x = w.innerWidth || e.clientWidth || g.clientWidth,
        y = w.innerHeight|| e.clientHeight|| g.clientHeight;
    
    this.el.style.width = x + "px";
    this.el.style.height = y + "px";
  },

  draw: function() {
    var pre = document.createElement("pre");
    var wrapClass = (this.wrap)? 'wrapped' : '';
    var languageClass = (this.language in sh_languages)? 'sh_' + this.language : this.language;
    this.el.innerHTML = '';
    this.el.appendChild(pre);
    
    // IE6 clear whitespaces with innerHTML, so use outerHTML instead
    pre.outerHTML = '<pre class="' + languageClass + ' ' + wrapClass +'">' + this.code + '</pre>';
    sh_highlightDocument();
    this.resize();
  },

  load: function(code, language, wrap){
    this.code = code;
    this.language = language;
    this.wrap = wrap;
    boxCode.draw();
  }
};

// global var
var boxCode = null;

// external functions
function loadBase64Code(base64Code, language, wrap) {
  boxCode.load(Base64.decode(base64Code), language, wrap);
}

function loadCode(code, language, wrap) {
  boxCode.load(code, language, wrap);
}

// init
boxCode = new BoxCode(document.getElementById('code-box'));