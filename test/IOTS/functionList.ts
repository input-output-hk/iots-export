import * as t from 'io-ts'

// IOTSSpec.User
const User = t.type({
    userId: t.Int,
    name: t.string
});

const SomeFunctionArgA = t.array(User);

const SomeFunctionArgReturn = t.string;

type SomeFunction = (
    a: t.TypeOf<typeof SomeFunctionArgA>
) => t.TypeOf<typeof SomeFunctionArgReturn>;
