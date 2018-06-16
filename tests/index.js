(function(){

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

    let testArrFuncResult = function() {

        getComplexSale();
        
        // should get [from, to, dt] completion getComplexSale().itinerary[0].<here>
        getComplexSale().itinerary[2];

        // should get array method completion getLineNumbers().<here>
        let lineNumbers = getLineNumbers();
        // should get array method completion lineNumbers.<here>
        lineNumbers;

        let pnr = {
            recordLocator: 'QWE123',
            ssrLineNumbers: getLineNumbers(),
        };
        // should get array method completion pnr.ssrLineNumbers.<here>
        pnr.ssrLineNumbers;
        // should get [major, minor, isOnlySsr] completion pnr.ssrLineNumbers[0].<here>
        pnr.ssrLineNumbers[0];
        pnr.names = {main: 'Vova', secondary: 'Lena'};
        // should get [main, secondary] completion pnr.names.<here>
        pnr.names;
    };
})();
