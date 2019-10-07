
const noBuiltIn = a => a;

const SomeEs6Module = noBuiltIn(() => {
    return {
        oolo: 101,
        aala: 102,
        uulu: 103,
    };
});

export const getList = () => {
    return [
        SomeEs6Module(),
        SomeEs6Module(),
        SomeEs6Module(),
    ];
};

export default SomeEs6Module;

const abc = noBuiltIn({a: 1, b: 2, c: 3});
const doStuff = noBuiltIn(() => ({stuff: 'is', done: '100%'}));

export {
    abc,
    doStuff,
}
