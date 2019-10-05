<?php
class ExactKeysUnitTest
{
    /** @param $arg = {asd1: 1, asd2: 2, asd3: 3} */
    public function provideSimpleObj($arg)
    {
        $arg[''];
        return [
            [$arg, ['asd1', 'asd2', 'asd3']],
        ];
    }

    /** @param $arg = await require('ExactKeysUnitTest.js').provideAsyncPromise() */
    public function provideAsyncPromise($arg)
    {
        $arg[''];
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
        $pnr['passengers']['passengerList'][0]['nameNumber']['absolute'];
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

    /** @param $arg = require('ExactKeysUnitTest.js').provideArrSpread() */
    public function provideArrSpread($arg)
    {
        $arg[0][''];
        return [
            [$arg[0], ['cmd', 'type']],
        ];
    }

    /** @param $arg = require('ExactKeysUnitTest.js').provideObjSpread() */
    public function provideObjSpread($arg)
    {
        $arg[''];
        return [
            [$arg, ['area', 'pcc', 'recordLocator', 'canCreatePqErrors']],
        ];
    }

    /** @param $arg = require('ExactKeysUnitTest.js').provideCmdLog() */
    public function provideCmdLog($arg)
    {
        $arg[''];
        $arg['desc'][0][''];
        $arg['copied'][0][''];
        $arg['reversed'][0][''];
        return [
            [$arg, ['desc', 'copied', 'reversed']],
            [$arg['desc'][0], ['desc', 'session_id','gds','type','is_mr','dt','cmd','duration','cmd_rq_id','area','record_locator','has_pnr','is_pnr_stored','output']],
            [$arg['copied'][0], ['copied', 'session_id','gds','type','is_mr','dt','cmd','duration','cmd_rq_id','area','record_locator','has_pnr','is_pnr_stored','output']],
            [$arg['reversed'][0], ['reversed', 'session_id','gds','type','is_mr','dt','cmd','duration','cmd_rq_id','area','record_locator','has_pnr','is_pnr_stored','output']],
        ];
    }

    /** @param $arg = require('ExactKeysUnitTest.js').provideObjInstructuring() */
    public function provideObjInstructuring($arg)
    {
        $arg[''];
        return [
            [$arg, ['id', 'context', 'gdsData']],
            [$arg['context'], ['agentId', 'gds']],
            [$arg['gdsData'], ['profile', 'token']],
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

    /** @param $pnr = require('ExactKeysUnitTest.js').provideFfInfoStackOverflow() */
    public function provideFfInfoStackOverflow($pnr)
    {
        $pnr[''];
        return [
            [$pnr, ['parsedData']],
            [$pnr['parsedData'], ['misc']],
        ];
    }

    /** @param $pnr = require('ExactKeysUnitTest.js').provideGalStackOverflow() */
    public function provideGalStackOverflow($pnr)
    {
        $pnr[''];
        return [
            [$pnr, ['foneData']],
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
            [$apoPnr['passengers']['passengerList'][0], ['lastName', 'firstName', 'nameNumber', 'success', 'parsedNumber', 'rawNumber', 'joinedFirstNames', 'ageGroup', 'age', 'dob', 'ptc', 'carrierText', 'remark']],
        ];
    }

    /** @param $arg = require('ExactKeysUnitTest.js').provideArrPropNotEl() */
    public function provideArrPropNotEl($arg)
    {
        $arg[''];
        return [
            [$arg['func'], ['canCall']],
            [$arg['el'], ['cmd', 'output']],
        ];
    }

    /** @param $arg = require('ExactKeysUnitTest.js').provideStaticMethodViaThis() */
    public function provideStaticMethodViaThis($arg)
    {
        $arg[''];
        return [
            [$arg, ['error', 'raw', 'parsed']],
            [$arg['parsed'], ['netPrice']],
        ];
    }

    /** @param $arg = require('ExactKeysUnitTest.js').provideWrongAssignmentDestination() */
    public function provideWrongAssignmentDestination($arg)
    {
        $arg[''];
        return [
            [$arg, ['noPricingPart']],
        ];
    }

    /** @param $arg = require('ExactKeysUnitTest.js').provideArgFromTsGeneric() */
    public function provideArgFromTsGeneric($arg)
    {
        $arg[''];
        return [
            [$arg, ['author', 'text']],
        ];
    }

    /** @param $arg = require('ExactKeysUnitTest.js').provideCircularGenerics() */
    public function provideCircularGenerics($arg)
    {
        $arg[''];
        return [
            [$arg, ['gotThroughCircularGenerics']],
        ];
    }

    /** @param $arg = require('ExactKeysUnitTest.js').provide15kDupeFqns() */
    public function provide15kDupeFqns($arg)
    {
        $arg[''];
        return [
            [$arg, ['id', 'ololo']],
        ];
    }

    /** @param $arg = require('ExactKeysUnitTest.js').providePrivateByFuncRef() */
    public function providePrivateByFuncRef($arg)
    {
        $arg[''];
        return [
            [$arg, ['ololo', 'lalala', 'lululu', 'kokoko']],
        ];
    }

    // ========================
    // following not implemented yet
    // ========================

    /** @param $arg = require('ExactKeysUnitTest.js').provideFlatMap() */
    public function provideFlatMap($arg)
    {
        $arg[''];
        return [
            //[$arg, ['asd', 'dsa']],
        ];
    }
}