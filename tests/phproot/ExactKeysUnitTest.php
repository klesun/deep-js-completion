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
            [$GdsProfiles, ['TRAVELPORT', 'AMADEUS', 'SABRE', 'getTravelport', 'getSabre', 'getAmadeus', 'getLimit', 'chooseAmaProfile']],
            [$GdsProfiles['TRAVELPORT'], ['DynApolloProd_1O3K', 'DynApolloCopy_1O3K', 'DynApolloProd_2F3K', 'DynApolloProd_2G55', 'DynGalileoProd_711M']],
            [$GdsProfiles['SABRE'], ['SABRE_PROD_L3II', 'SABRE_PROD_Z2NI', 'SABRE_PROD_6IIF', 'SABRE_PROD_8ZFH']],
            [$GdsProfiles['AMADEUS'], ['AMADEUS_TEST_1ASIWTUTICO', 'AMADEUS_PROD_1ASIWTUTICO', 'AMADEUS_PROD_1ASIWTUT0GW', 'AMADEUS_PROD_1ASIWTUTDTT']],
        ];
    }

    /** @param $pnr = require('ExactKeysUnitTest.js').provideParseApoPnr() */
    public function provideParseApoPnr($pnr)
    {
        $pnr['dataExistsInfo'];
        $pnr['passengers']['passengerList'][0][''];
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

    /** @param $arg = require('ExactKeysUnitTest.js').provideForInExistingVar() */
    public function provideForInExistingVar($arg)
    {
        $arg[''];
        return [
            [$arg, ['hi', 'how']],
            [$arg['how'], ['are', 'doing']],
        ];
    }

    /** @param $arg = require('ExactKeysUnitTest.js').providePushSpread() */
    public function providePushSpread($arg)
    {
        $arg[0][''];
        return [
            [$arg[0], ['huj', 'pizda']],
        ];
    }

    /** @param $arg = require('ExactKeysUnitTest.js').provideArgSpread() */
    public function provideArgSpread($arg)
    {
        $arg[0][''];
        return [
            [$arg[0], ['name', 'age', 'height', 'salary']],
        ];
    }

    /** @param $arg = require('ExactKeysUnitTest.js').provideArrayMerge() */
    public function provideArrayMerge($arg)
    {
        $arg['mergedObj'][''];
        $arg['mergedArr'][0][''];
        return [
            [$arg['mergedObj'], ['a', 'b', 'c', 'd', 'e']],
            [$arg['mergedArr'][0], ['f', 'g']],
        ];
    }

    /** @param $arg = require('ExactKeysUnitTest.js').provideSelfDependency() */
    public function provideSelfDependency($arg)
    {
        $arg[''];
        $arg['passengerList'][0]['nameNumber'][''];
        return [
            [$arg, ['passengerList']],
            [$arg['passengerList'][0], ['lastName', 'firstName', 'ptc', 'nameNumber']],
            [$arg['passengerList'][0]['nameNumber'], ['lastNameNumber', 'firstNameNumber', 'absolute']],
        ];
    }

    // ========================
    // following not implemented yet
    // ========================

    /** @param $pnr = require('ExactKeysUnitTest.js').provideFfInfoStackOverflow() */
    public function provideFfInfoStackOverflow($pnr)
    {
        $pnr[''];
        return [
            [$pnr, ['parsedData']],
            [$pnr['parsedData'], ['misc']],
        ];
    }

    /**
     * @param $pnr = require('PnrParser.js').parse()
     * @param $apoPnr = require('ExactKeysUnitTest.js').provideParseApoPnr()
     */
    public function provideParsePnrOutOfMemory($pnr, $apoPnr)
    {
        $pnr['dataExistsInfo'];
        $pnr['passengers']['passengerList'][0][''];
        $apoPnr[''];
        $apoPnr['passengers']['passengerList'][0][''];
        return [
            [$apoPnr['passengers'], ['passengerList', 'pnrIsCurrentlyInUse', 'reservationInfo', 'shopInfo', 'agentName']],
            [$apoPnr['passengers']['passengerList'][0], ['lastName', 'firstName', 'nameNumber']],
        ];
    }
}