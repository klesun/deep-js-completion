
let SomeCjsModule = require('./SomeCjsModule.js');

SomeCjsModule({}).runInputCmd('*R').then(resp => {
    resp.o;
});

let makeCmdResponse = (data) => 1 && {
    success: true,
    data: Object.assign({
        output: 'NO RESPONSE',
        tabCommands: [],
        clearScreen: false,
        canCreatePq: false,
        canCreatePqErrors: [],
        area: 'A',
        pcc: '1O3K',
        prompt: "",
        startNewSession: false,
        userMessages: null,
        appliedRules: [],
        legend: [],
    }, data),
};

let whenFetched = fetch('https://google.com')
    .then(resp => resp.text());

let whenRules = Promise.resolve({len: 5, appliedRules: [{id: 123, decoration: ['italic']}], patterns: [{ruleId: 123, value: "%  3 KL %"}]});

let whenTime = whenRules.then(rules => {
    return {time: Math.random(), rules: rules};
});

let running = whenTime.then(makeCmdResponse)
    .then(resp => resp.);

running.then(timed => {
    timed.;
});

(function(){

    let ssrLineNumbers = [];

    var klesun = Klesun();
    klesun.requires('./Tls').then = Tls =>
    klesun.whenLoaded = () => (...ctorParams) => {
        let tls = Tls();
        // should suggest: opt, promise, http, mkDom, range, deepCopy
        tls.o;
    };

    define(['./Tls.js'], (Tls) => (...ctorArgs) => {
        let tls = Tls();
        // should suggest: opt, promise, http, mkDom, range, deepCopy
        tls.a;
    });

    /**
     * @typedef {Object} generatedPoint
     * @type {Object}
     * @property {number} x The x coordinate.
     * @property {number} y The y coordinate.
     * @property {number} amount
     */

    let a = 5,
        b = 6;

    /** @param whiskey {generatedPoint} */
    let Denis = function(whiskey){
        return {
            monthsTilDiploma: 12 + whiskey.amount / 1000,
            takeTripleShot: () => alert('I aint drunk yeaaaat'),
            dropTheLocker: () => alert('Bam!'),
        };
    };

    setTimeout(() => {
        /**
         * @param denis = Denis()
         * @param tls = from('./Tls.js')()
         * @param smf {from('./SmfAdapter.js')()}
         * @param pax {{first: 'Vova', last: 'Pupkin', age: '18'}}
         * @param segment {{a: string, b: int}}
         * @param segment.from
         * @param segment.to
         */
        let testArgDocComment = function(denis, tls, smf, pax, segment) {
            // should suggest: yearsTilDiploma, takeTripleShot, dropTheLocker
            denis.t;
            // should suggest: first, last, age
            pax.f;
            // should suggest: opt, promise, http, mkDom, range, deepCopy
            tls.o;
        };
    }, 100);

    let collectSmfNotes = function(smf) {
        let notes = [];
        notes.push({zalupa:123});
        let otherEvents = [];
        let chanToNotes = range(0,16).map(i => []);
        for (let [i, track] of Object.entries(smf.tracks)) {
            let time = 0;
            let noteOnIndex = 0;
            for (let event of track.events) {
                time += event.delta;
                if (isNoteOn(event)) {
                    let chan = event.midiChannel;
                    let tone = event.parameter1;
                    let velo = event.parameter2;
                    let dura = 0;
                    chanToNotes[chan][tone] = chanToNotes[chan][tone] || [];
                    chanToNotes[chan][tone].push({
                        tone, time, dura, chan, track: i,
                        velo, index: noteOnIndex++,
                    });
                } else if (isNoteOff(event)) {
                    let chan = event.midiChannel;
                    let tone = event.parameter1;
                    for (let note of chanToNotes[chan][tone] || []) {
                        note.dura = time - note.time;
                        notes.push(note);
                    }
                    chanToNotes[chan][tone] = [];
                } else {
                    otherEvents.push({time, event, track: i});
                }
            }
        }
        notes[0].a;
        return {notes, otherEvents};
    };

    let testPushInference = function() {
        let collected = collectSmfNotes();
        // should suggest: zalupa, dura, tone, time, chan, track, velo, index
        collected.notes[0].z;
    };

    let testMiscStuff = function() {
        let getObj = function () {
            return {
                lol: 5,
                ele: 6,
                ulu: {
                    ara: 5,
                    boro: 7,
                }
            };
        };

        var getWrappedObj = () => {
            return {
                huj: 5,
                pizda: getObj(),
                zalupa: [{
                    asdasda: 123
                }],
            };
        };

        var someUnrelatedObj = {
            c: 8, d: 9,
        };

        someUnrelatedObj.a;

        var someArr = [
            {a: 5, b: 6},
            {a: 5, b: 6},
            {a: 5, b: 6},
            {a: 5, b: 6},
        ];

        for (let obj of someArr) {
            obj.a;
        }

        someArr[0].;

        var obj = getObj();
        obj.;
        obj.ulu.;


        getWrappedObj().zalupa[0].asdasda;
    };

    let getComplexSale = function() {
        if (Math.random() > 0.5) {
            return {error: 'You are not lucky'};
        }
        return {
            netPrice: {currency: 'EUR', amount: 290.00},
            itinerary: [
                {from: 'KIV', to: 'RIX', dt: '2018-05-13 20:00:00'},
                {from: 'RIX', to: 'KIB', dt: '2018-05-20 20:00:00'},
                {from: 'RIX', to: 'KIB', dt: '2018-05-20 20:00:00', oloolo: 'asd'},
            ],
        };
    };

    let getLineNumbers = function() {
        if (Math.random() > 0.5) {
            return [{major: 1, isOnlySsr: true}];
        }

        return [
            {major: 1, minor: 5, },
            {major: 1, minor: 6, },
            {major: 2, minor: 1, },
        ];
    };

    let getVeryComplexSale = () => {
        let sale = getComplexSale();
        sale.currency = 'USD';
        return sale;
    };

    let getBinaryOp = () => 1 && {
        name: 'Vasya',
        age: 19,
        nationality: 'latvian',
    };

    let testArrFuncResult = function() {

        getBinaryOp().n;

        let sale = getVeryComplexSale();
        sale.currency;

        let delayedData = null;
        setTimeout(() => {
            delayedData.responses.map(a => a);
        }, 100);
        delayedData = {
            id: 123,
            type: 'comment',
            content: 'ololo 123',
            responses: [{a: 5}, {n:6}],
        };

        // should get [from, to, dt] completion getComplexSale().itinerary[0].<here>
        getComplexSale().itinerary[2];

        // should get [a, b] completion obj..<here>
        [{a:5, b:6}, {a:6, b:3}].forEach(obj => {
            obj.a;
        });

        // should get array method completion getLineNumbers().<here>
        let lineNumbers = getLineNumbers();
        // should get array method completion lineNumbers.<here>
        lineNumbers.forEach(a => a.isOnlySsr);

        let zhopa = {ssrLineNumbers: [{huj: 123, isOnlySsr: 123}]};

        let pnr = {
            recordLocator: 'QWE123',
            ssrLineNumbers: getLineNumbers(),
        };
        // should get array method completion pnr.ssrLineNumbers.<here>
        pnr.ssrLineNumbers.map(a => a.isOnlySsr);
        // should get [major, minor, isOnlySsr] completion pnr.ssrLineNumbers[0].<here>
        pnr.ssrLineNumbers[0];
        pnr.names = [{main: 'Vova', secondary: 'Lena'}];
        // should get [main, secondary] completion pnr.names[0].<here>
        pnr.names.map(a => a.main);

        let {recordLocator, ssrLineNumbers} = pnr;
        ssrLineNumbers[0].isOnlySsr;
    };
})();
