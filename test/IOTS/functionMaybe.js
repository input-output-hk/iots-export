"use strict";
exports.__esModule = true;
var t = require("io-ts");
// Schema.IOTSSpec.Slot
var Slot = t.type({
    getSlot: t.Int
});
var someFunctionArgA = t.union([
    Slot,
    t["null"]
]);
var someFunctionReturn = t.string;
