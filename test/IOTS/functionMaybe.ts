import * as t from 'io-ts'

// IOTSSpec.Slot
const Slot = t.type({
    getSlot: t.Int
});

const SomeFunctionArgA = t.union([
    Slot,
    t.null
]);

const SomeFunctionArgReturn = t.string;

type SomeFunction = (
    a: t.TypeOf<typeof SomeFunctionArgA>
) => t.TypeOf<typeof SomeFunctionArgReturn>;
