
let php = require('./../unv/grect/backend/Transpiled/php.js');

exports.provideAsyncPromise = async () => {
    return Promise.resolve({
        zxc1: 123,
        zxc2: 123,
        zxc3: 123,
    });
};

exports.providePushSpread = () => {
    let result = [];
    let more = [{huj: 1, pizda: 2}];
    result.push(...more);
    result[0].h;
    return result;
};

exports.provideObjectEntries = () => {
    let srcObj = {aa: 5, bb:6, cc:7};
    let result = {};
    for (let [k,v] of Object.entries(srcObj)) {
        result[k] = v;
    }
    return result;
};

let makeArr = (...objs) => {
    let result = [];
    for (let obj of objs) {
        result.push(obj);
    }
    return result;
};

exports.provideArgSpread = () => {
    let arr = makeArr(
        {name: 'Vasya', age: 15},
        {name: 'Lena', height: 175},
        {name: 'Ksyusha', salary: 800},
    );
    arr[0].;
};

exports.provideArrayMerge = () => {
    // there is push(...arr) inside - ... part should not be ignored
    let mergedObj = php.array_merge({a:1,b:2,c:3}, {d:4,e:5});
    mergedObj.a;
    let mergedArr = php.array_merge([{f:5}], [{g:6}]);
    mergedArr[0].a;
    return {
        mergedObj: mergedObj,
        mergedArr: mergedArr,
    }
};
