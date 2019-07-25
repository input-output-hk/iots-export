"use strict";
exports.__esModule = true;
var t = require("io-ts");
// Schema.IOTSSpec.CurrencySymbol
var CurrencySymbol = t.type({
    unCurrencySymbol: t.string
});
// Schema.IOTSSpec.TokenName
var TokenName = t.type({
    unTokenName: t.string
});
var someFunctionArgA = t.tuple([
    CurrencySymbol,
    TokenName
]);
var someFunctionReturn = t.string;
