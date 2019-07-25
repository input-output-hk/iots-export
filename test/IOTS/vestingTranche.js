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
// Schema.IOTSSpec.AssocMap
var AssocMapTokenNameInteger = t.type({
    unMap: t.array(t.tuple([
        TokenName,
        t.Int
    ]))
});
// Schema.IOTSSpec.AssocMap
var AssocMapCurrencySymbolAssocMapTokenNameInteger = t.type({
    unMap: t.array(t.tuple([
        CurrencySymbol,
        AssocMapTokenNameInteger
    ]))
});
// Schema.IOTSSpec.Value
var Value = t.type({
    getValue: AssocMapCurrencySymbolAssocMapTokenNameInteger
});
// Schema.IOTSSpec.Slot
var Slot = t.type({
    getSlot: t.Int
});
// Schema.IOTSSpec.Interval
var IntervalSlot = t.type({
    ivFrom: t.union([
        Slot,
        t["null"]
    ]),
    ivTo: t.union([
        Slot,
        t["null"]
    ])
});
// Schema.IOTSSpec.VestingTranche
var VestingTranche = t.type({
    vestingTrancheDate: Slot,
    vestingTrancheAmount: Value,
    validity: IntervalSlot
});
var someFunctionArgA = t.tuple([
    CurrencySymbol,
    TokenName
]);
var someFunctionArgB = t.union([
    Value,
    t["null"]
]);
var someFunctionArgC = IntervalSlot;
var someFunctionArgD = t.array(VestingTranche);
var someFunctionReturn = t.string;
