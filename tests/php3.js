

let php = {};

php.array_slice = (arr, start, length = undefined) => {
	arr = Object.values(arr);
	if (start < 0) {
		start = arr.length + start;
	}
	length = length === undefined ? arr.length : length;
	return arr.slice(start, start + length);
};

// ------------------
//  functional built-ins follow
// ------------------

let normFunc = (func) => {
	if (typeof func === 'string') {
		if (func in php) {
			func = php[func];
		} else {
			throw Error('Unsupported built-in function - ' + func);
		}
	}
	return func;
};
php.array_map = (func, obj, additionalValues = []) => {
	func = normFunc(func);
	let newObj = Array.isArray(obj) ? [] : {};
	for (let [key, val] of Object.entries(obj)) {
		newObj[key] = func(val, additionalValues[key]);
	}
	return newObj;
};

//php.PREG_OFFSET_CAPTURE = 256;

php.PHP_EOL = '\n';

module.exports = php;
