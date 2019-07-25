"use strict";
exports.__esModule = true;
var t = require("io-ts");
// Schema.IOTSSpec.Response
var ResponseString = t.union([
    t.type({ "UnknownError": t.tuple([
            t.Int,
            t.string
        ]) }),
    t.type({ "StatusError": t.type({
            code: t.Int,
            message: t.string,
            headers: t.record(t.string, t.Int)
        }) }),
    t.literal('EmptyResponse'),
    t.type({ "Response": t.string })
]);
