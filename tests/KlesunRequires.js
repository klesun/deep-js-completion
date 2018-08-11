/**
 * this is a handy wrapper for require.js
 * are you tired of specifying module paths in one
 * array, and matching resolved values in another?
 * then this util is for you:
 *
 * Before:
 * define(['A.js', 'B.js', 'E.js', 'D.js'], (A, B, E, D) => {...})
 *
 * After:
 * klesun.requires('A.js').then = A =>
 * klesun.requires('B.js').then = B =>
 * klesun.requires('E.js').then = E =>
 * klesun.requires('D.js').then = D =>
 * klesun.whenLoaded = () => {...}
 *
 * Note that each module is supposed to return a function, so this
 * util will be useful only for local project module resolution
 */
var Klesun = Klesun || (() => {
    let modulePaths = [];
    let loadedClasses = [];
    return 1 && {
        requires: function (modulePath) {
            modulePaths.push(modulePath);
            let i = modulePaths.length - 1;
            let makeInstance = (...args) => {
                let module = loadedClasses[i];
                if (typeof module === 'function') {
                    // module is a function (class) - call it with passed params
                    return module(...args);
                } else {
                    // module is a static object - just return it
                    return module;
                }
            };
            return {
                set then(callback) {
                    callback(makeInstance);
                }
            };
        },
        set whenLoaded(finalCallback) {
            define(modulePaths, (...modules) => {
                loadedClasses.push(...modules);
                return finalCallback();
            });
        },
    };
});