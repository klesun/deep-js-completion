
import {Component} from 'react';

let php = require('./../unv/grect/backend/Transpiled/php.js');
let php2 = require('../php.js');
let php3 = require('../php3.js');
const ApoPnrParser = require("../unv/grect/backend/Transpiled/Gds/Parsers/Apollo/Pnr/PnrParser");
const SabPnrParser = require("../unv/grect/backend/Transpiled/Gds/Parsers/Sabre/Pnr/PnrParser");
const AmaPnrParser = require("../unv/grect/backend/Transpiled/Gds/Parsers/Amadeus/Pnr/PnrParser");
const GalPnrParser = require("../unv/grect/backend/Transpiled/Gds/Parsers/Galileo/Pnr/PnrParser");
const CmdLogs = require('../unv/grect/backend/Repositories/CmdLogs.js');

class OfficeCompletion extends Component
{
    constructor () {
        super();
        this.state = {
            formData : {
                userOffices : [1, 5, 7],
                advancePurchase : {
                    units : "days",
                    amount : 0
                }
            },
            open : false,
        };
        this.state.formData = 123;
        this.state.formData;
    }

    render () {
        const {formData} = this.state;
        const {advancePurchase} = formData;

        console.log( formData.s ); // <== zdesj suggest (advancePurchase, userOffices)
    }
}

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
    parsed.parsed.passengers[0].nameNumber.;
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

let noBuiltIn = val => val;

exports.provideArrSpread = () => {
    let cmdRecs = noBuiltIn([
        {cmd: 'A10MAYJFKMNL', type: 'airAvailability'},
        {cmd: '01N1*GK', type: 'sell'},
        {cmd: '$BB0', type: 'priceItinerary'},
    ]);
    let copied = [...cmdRecs];
    copied[0];
    return copied;
};

exports.provideObjSpread = () => {
    let sessionState = noBuiltIn({
        area: 'A',
        pcc: '2N3K',
        recordLocator: 'QWE123',
    });
    let copied = {
        ...sessionState,
        canCreatePqErrors: ['No recent valid pricing'],
    };
    copied.a;
    return copied;
};

exports.provideCmdLog = async () => {
    let desc = await CmdLogs.getAll(123);
    desc[0].c;
    let copied = [...desc, undefined];
    copied[0].c;
    let reversed = copied.reverse();
    reversed[0].c;
    return {
        desc: [...desc].concat([{desc: true}]),
        copied: [...copied].concat([{copied: true}]),
        reversed: [...reversed].concat([{reversed: true}]),
    };
};

exports.provideObjInstructuring = () => {
    let context = {agentId: 6206, gds: 'apollo'};
    let id = 1235;
    let gdsData = {profile: 'OLOLO', token: 'qwe123sdg345'};
    let sessionRecord = noBuiltIn({id, context, gdsData});
    sessionRecord.c;
    return sessionRecord;
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

exports.provideArrPropNotEl = () => {
    let cmdRecs = [
        {cmd: '*R', output: 'INVLD \n><'},
        {cmd: '$B', output: 'NO VALID FARE FOR INPUT CRITERIA \n><'},
    ].map(a => a);

    let el = cmdRecs.shift();
    el.;

    // should not suggest cmd/output as this is a property, not an element
    cmdRecs.shift.c;
    let func = cmdRecs.shift;
    func.canCall = false;
    return {func, el};
};

/** @param pax = require('PnrParser.js').parse().passengers.passengerList[0] */
let provideParsePnrOutOfMemory = (pax) => {
    pax.nameNumber;
};
