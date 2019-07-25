"use strict";
exports.__esModule = true;
var t = require("io-ts");
// Ledger.Value
var CurrencySymbol = t.type({
    unCurrencySymbol: t.string
});
// Ledger.Slot
var Slot = t.type({
    getSlot: t.Int
});
// Ledger.Crypto
var PubKey = t.type({
    getPubKey: t.string
});
// Ledger.Value
var TokenName = t.type({
    unTokenName: t.string
});
// Ledger.Value
var Value = t.type({
    getValue: t.record(CurrencySymbol, t.record(TokenName, t.Int))
});
// Main
var Campaign = t.type({
    campaignDeadline: Slot,
    campaignTarget: Value,
    campaignCollectionDeadline: Slot,
    campaignOwner: PubKey
});
// Main
var CampaignAction = t.union([
    t.literal('Collect'),
    t.literal('Refund')
]);
