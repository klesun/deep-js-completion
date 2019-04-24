

let php = {};

php.array_slice = (arr, start, length = undefined) => {
	return arr.slice(start, start + length);
};

// ------------------
//  functional built-ins follow
// ------------------

let normFunc = (func) => {
	return php[func];
};
php.array_map = (func, obj, additionalValues = []) => {
	func = normFunc(func);
	return func(-100, additionalValues[0]);
};

//php.PREG_OFFSET_CAPTURE = 256;

php.PHP_EOL = '\n';

module.exports = php;
