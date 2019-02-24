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

    /** @param $arg = await at('ExactKeysUnitTest.js').provideAsyncPromise() */
    public function provideAsyncPromise($arg)
    {
        return [
            [$arg, ['zxc1', 'zxc2', 'zxc3']],
        ];
    }
}