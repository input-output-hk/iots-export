import * as t from 'io-ts'

// IOTSSpec.CurrencySymbol
const CurrencySymbol = t.type({
    unCurrencySymbol: t.string
});

// IOTSSpec.TokenName
const TokenName = t.type({
    unTokenName: t.string
});

// IOTSSpec.AssocMap
const AssocMapTokenNameInteger = t.type({
    unMap: t.array(t.tuple([
        TokenName,
        t.Int
    ]))
});

// IOTSSpec.AssocMap
const AssocMapCurrencySymbolAssocMapTokenNameInteger = t.type({
    unMap: t.array(t.tuple([
        CurrencySymbol,
        AssocMapTokenNameInteger
    ]))
});

// IOTSSpec.Value
const Value = t.type({
    getValue: AssocMapCurrencySymbolAssocMapTokenNameInteger
});

// IOTSSpec.Slot
const Slot = t.type({
    getSlot: t.Int
});

// IOTSSpec.Interval
const IntervalSlot = t.type({
    ivFrom: t.union([
        Slot,
        t.null
    ]),
    ivTo: t.union([
        Slot,
        t.null
    ])
});

// IOTSSpec.VestingTranche
const VestingTranche = t.type({
    vestingTrancheDate: Slot,
    vestingTrancheAmount: Value,
    validity: IntervalSlot
});

const SomeFunctionArgA = t.tuple([
    CurrencySymbol,
    TokenName
]);

const SomeFunctionArgB = t.union([
    Value,
    t.null
]);

const SomeFunctionArgC = IntervalSlot;

const SomeFunctionArgD = t.array(VestingTranche);

const SomeFunctionArgReturn = t.string;

type SomeFunction = (
    a: t.TypeOf<typeof SomeFunctionArgA>,
    b: t.TypeOf<typeof SomeFunctionArgB>,
    c: t.TypeOf<typeof SomeFunctionArgC>,
    d: t.TypeOf<typeof SomeFunctionArgD>
) => t.TypeOf<typeof SomeFunctionArgReturn>;
