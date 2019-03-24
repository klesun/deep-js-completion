
let php = {};

php.array_combine = (keys, values) => {
	keys = Object.values(keys);
	values = Object.values(values);
	if (keys.length !== values.length) {
		throw new Error('array_combine passed key count ' + keys.length +
			' does not match value count ' + values.length);
	}
	let result = {};
	for (let i = 0; i < keys.length; ++i) {
		result[keys[i]] = values[i];
	}
	return result;
};

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

module.exports = php;
