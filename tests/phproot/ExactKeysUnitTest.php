<?php
class ExactKeysUnitTest
{
    /** @param $arg = {asd1: 1, asd2: 2, asd3: 3} */
    public function provideSimpleObj($arg)
    {
        return [
            [$arg, ['asd1', 'asd2', 'asd3']],
        ];
    }

    /** @param $arg = await require('ExactKeysUnitTest.js').provideAsyncPromise() */
    public function provideAsyncPromise($arg)
    {
        return [
            [$arg, ['zxc1', 'zxc2', 'zxc3']],
        ];
    }

    /** @param $GdsProfiles = require('GdsProfiles.js') */
    public function provideOtherArgsCache($GdsProfiles)
    {
        $GdsProfiles['AMADEUS'][''];
        return [
            [$GdsProfiles, ['TRAVELPORT', 'AMADEUS', 'SABRE', 'getTravelport', 'getSabre', 'getAmadeus']],
            [$GdsProfiles['TRAVELPORT'], ['DynApolloProd_1O3K', 'DynApolloCopy_1O3K', 'DynApolloProd_2F3K', 'DynApolloProd_2G55', 'DynGalileoProd_711M']],
            [$GdsProfiles['SABRE'], ['SABRE_PROD_L3II', 'SABRE_PROD_Z2NI', 'SABRE_PROD_6IIF', 'SABRE_PROD_8ZFH']],
            [$GdsProfiles['AMADEUS'], ['AMADEUS_TEST_1ASIWTUTICO', 'AMADEUS_PROD_1ASIWTUTICO', 'AMADEUS_PROD_1ASIWTUT0GW']],
        ];
    }

    /** @param $pnr = require('PnrParser.js').parse() */
    public function provideParsePnr($pnr)
    {
        $pnr['dataExistsInfo'];
        $pnr['passengers'][0][''];
        return [
            [$pnr, ['dataExistsInfo', 'headerData', 'passengers', 'itineraryData', 'foneData', 'adrsData', 'dlvrData', 'formOfPaymentData', 'tktgData', 'atfqData', 'ticketListData', 'ssrData', 'remarks', 'acknData']],
        ];
    }

    /** @param $arg = require('ExactKeysUnitTest.js').provideObjectEntries() */
    public function provideObjectEntries($arg)
    {
        $arg[''];
        return [
            [$arg, ['aa', 'bb', 'cc']],
        ];
    }

    // ========================
    // following not implemented yet
    // ========================

    /** @param $arg = require('ExactKeysUnitTest.js').provideArrayMerge() */
    public function provideArrayMerge($arg)
    {
        $arg['mergedObj'][''];
        $arg['mergedArr'][0][''];
        return [
            //[$arg['mergedObj'], ['a', 'b', 'c', 'd', 'e']],
            //[$arg['mergedArr'][0], ['f', 'g']],
        ];
    }
}