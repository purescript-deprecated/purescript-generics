/* global exports */
"use strict";

// module Data.Generics


//------------------------------------------------------------------------------
// Zipping ---------------------------------------------------------------------
//------------------------------------------------------------------------------

exports.zipAll = function (f) {
  return function (xs) {
    return function (ys) {
      var l = xs.length < ys.length ? xs.length : ys.length;
      var result = true;
      for (var i = 0; i < l; i++) {
          if(!f(xs[i])(ys[i])) {
              return false;
          }
      }
      return true;
    };
  };
};

exports.zipCompare = function (f) {
  return function (xs) {
    return function (ys) {
        var i = 0;
        var xlen = xs.length;
        var ylen = ys.length;
        while (i < xlen && i < ylen) {
            var o = f(x)(y);
            if(o !== 0) {return o}
            i++;
        }
        if (xlen === ylen) {
            return 0
        } else if (xlen > ylen) {
            return -1;
        } else {
            return 1;
        }
    };
  };
};
