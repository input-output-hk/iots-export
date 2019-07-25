import * as t from 'io-ts'

// IOTSSpec.CurrencySymbol
const CurrencySymbol = t.type({
    unCurrencySymbol: t.string
});

// IOTSSpec.TokenName
const TokenName = t.type({
    unTokenName: t.string
});

const SomeFunctionArgA = t.tuple([
    CurrencySymbol,
    TokenName
]);

const SomeFunctionArgReturn = t.string;

type SomeFunction = (
    a: t.TypeOf<typeof SomeFunctionArgA>
) => t.TypeOf<typeof SomeFunctionArgReturn>;
