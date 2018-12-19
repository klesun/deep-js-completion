let SomeCjsModule = (reqBody) => {
    return {
        doStuff: (arg1) => {
            return {lalala: 123};
        },
        runInputCmd: (cmd) => Promise.resolve({output: 'EXECUTED'})
    };
};

module.exports = SomeCjsModule;