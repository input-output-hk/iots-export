"use strict";
exports.__esModule = true;
var t = require("io-ts");
// Schema.IOTSSpec.SimpleSum
var SimpleSum = t.union([
    t.literal('This'),
    t.literal('That'),
    t.literal('TheOther')
]);
