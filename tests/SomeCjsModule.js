let SomeCjsModule = (reqBody) => {
    return {
        runInputCmd: (cmd) => Promise.resolve({output: 'EXECUTED'})
    };
};

module.exports = SomeCjsModule;