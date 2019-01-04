let SomeCjsModule = (reqBody) => {
    return {
        doStuff: (arg1) => {
            return {lalala: 123};
        },
        runInputCmd: (cmd) => Promise.resolve({output: 'EXECUTED'}),
        getSimpleObj: () => ({a: 5, b: 6}),
    };
};

module.exports = SomeCjsModule;