
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

function getSomeOtherArr () {
    return [1,2,3];
    //return {zalupa:5};
    // return [
    //     {f: 1, g: 6},
    // ];
}

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
var otherArr = getSomeOtherArr().;
otherArr[0].;

var obj = getObj();
obj.;
obj.ulu.;


getWrappedObj().zalupa[0].asdasda;