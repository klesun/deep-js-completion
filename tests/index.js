
function getSomeOtherArr () {
    return [
        {f: 1, g: 6},
    ];
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

someArr[0];
var otherArr = getSomeOtherArr();
otherArr[0].;