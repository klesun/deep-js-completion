
let php = require('./../unv/grect/backend/Transpiled/php.js');
let php2 = require('../php.js');
let php3 = require('../php3.js');
const ApoPnrParser = require("../unv/grect/backend/Transpiled/Gds/Parsers/Apollo/Pnr/PnrParser");
const SabPnrParser = require("../unv/grect/backend/Transpiled/Gds/Parsers/Sabre/Pnr/PnrParser");
const AmaPnrParser = require("../unv/grect/backend/Transpiled/Gds/Parsers/Amadeus/Pnr/PnrParser");
const GalPnrParser = require("../unv/grect/backend/Transpiled/Gds/Parsers/Galileo/Pnr/PnrParser");

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

exports.provideParseApoPnr = () => {
    let parsed = ApoPnrParser.parse('ASD ASD');
    parsed.passengers.passengerList[0].;
    return parsed;
};

exports.provideParseSabPnr = () => {
    let parsed = SabPnrParser.parse('ASD ASD');
    parsed.parsedData.passengers.parsedData.passengerList[0].nameNumber.r;
    return parsed;
};

exports.provideParseAmaPnr = () => {
    let parsed = AmaPnrParser.parse('ASD ASD');
    parsed.passengers.passengerList[0];
    return parsed;
};

exports.provideParseGalPnr = () => {
    let parsed = GalPnrParser.parse('ASD ASD');
    parsed.passengers.passengerList[0].;
    return parsed;
};


class Fp {
    static map($function, $arr) {
        return php2.array_map($function, $arr);
    }
}

exports.provideFfInfoStackOverflow = () => {
    // taken from Sabre PNR parser
    let $result, $sections;

    $result = {
        'parsedData': {
            'misc': null,
        },
    };

    let $lines = Fp.map(($line) => php.trim($line), []);
    $result['parsedData']['misc'] = {
        'ffDataExists': $lines.includes('FREQUENT TRAVELER DATA EXISTS *FF TO DISPLAY ALL'),
    };
    $result['parsedData']['misc']['ffDataExists'] =
        $result['parsedData']['misc']['ffDataExists'] ||
        php.count((($result['parsedData'] || {})['frequentTraveler'] || {})['mileagePrograms'] || []) > 0;

    $result.p;
    return $result;
};

exports.provideGalStackOverflow = ($dump) => {
    let $result, $sections, $parsedHead;

    // remove scrolling indicator if any
    $result = {};
    $result['foneData'] = false ? [] :
        php3.array_map('asd', StringUtil.lines('asd'));

    php3.array_map('asd').;

    $result.;
    return $result;
}

exports.provideObjectEntries = () => {
    let srcObj = {aa: 5, bb:6, cc:7};
    let result = {};
    for (let [k,v] of Object.entries(srcObj)) {
        result[k] = v;
    }
    result.a;
    return result;
};

exports.provideForInExistingVar = () => {
    let k, v;
    let source = [
        ['hi', 'dude'],
        ['how', {
            are: 'you',
            doing: 'today',
        }],
    ];
    let result = {};
    for ([k,v] of source) {
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
    arr[0].n;
    return arr;
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

exports.provideSelfDependency = () => {
    let $parsedData = {};
    let $result = {
        passengerList: [{
            lastName: 'Vasya',
            firstName: 'Pupkin',
            nameNumber: {
                lastNameNumber: 3,
                firstNameNumber: 2,
                absolute: 5,
            },
            ptc: 'Y13',
        }],
    };
    $parsedData['passengerList'] = php.array_merge($parsedData['passengerList'], $result['passengerList']);
    return $parsedData;
};

/** @param pax = require('PnrParser.js').parse().passengers.passengerList[0] */
let provideParsePnrOutOfMemory = (pax) => {
    pax.nameNumber;
};
