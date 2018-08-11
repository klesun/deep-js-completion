
/**
 * provides various coding tools like Optional-s and Promise-s
 */
define([], () => (...ctorArgs) => {

    /** similar to Java's Optional
     * @param forceIsPresent - use when value may be null (when it's not typed for example) */
    let opt = function (value, forceIsPresent = false) {
        let has = () => forceIsPresent ||
            value !== null && value !== undefined;
        let self;
        return self = {
            map: (f) => has() ? opt(f(value)) : opt(null),
            /** flat map - return empty or mapped array*/
            fmp: (f) => has() ? f(value) : [],
            flt: (f) => has() && f(value) ? opt(value) : opt(null),
            saf: (f) => {
                if (has()) {
                    try { return opt(f(value)); }
                    catch (exc) { console.error('Opt mapping threw an exception', exc); }
                }
                return opt(null);
            },
            def: (def) => has() ? value : def,
            has: has,
            set get(cb) { if (has()) { cb(value); } },
            wth: (f) => {
                if (has()) { f(value); }
                return self;
            },
            uni: (some, none) => has() ? some(value) : none(),
            err: (none) => {
                if (has()) {
                    return { set els(some) { some(value); } };
                } else {
                    none();
                    return { set els(some) { } };
                }
            },
        };
    };

    /**
     * similar to the built-in Promise, I guess,
     * but in slightly more convenient format
     */
    let promise = function(giveMemento)
    {
        let done = false;
        let result;
        let thens = [];
        giveMemento(r => {
            done = true;
            result = r;
            thens.forEach((cb) => cb(result));
        });
        let self = {
            set then(receive) {
                if (done) {
                    receive(result);
                } else {
                    thens.push(receive);
                }
            },
            map: (f) => promise(
                delayedReturn => self.then =
                    (r) => delayedReturn(f(r))
            ),
        };
        return self;
    };

    /** @param responseType = 'arraybuffer' | 'json' | 'text' */
    let http = (url, responseType) => promise(done => {
        let oReq = new XMLHttpRequest();
        oReq.open("GET", url, true);
        if (responseType) {
            oReq.responseType = responseType;
        }
        oReq.onload = () => done(oReq.response);
        oReq.send(null);
    });

    /** a shorthand to create dom elements in js with one expression */
    let mkDom = (tagName, params) => {
        let dom = document.createElement(tagName);
        for (let [k,v] of Object.entries(params || {})) {
            if (k === 'innerHTML') {
                dom.innerHTML = v;
            } else if (k === 'children') {
                v.forEach(c => dom.appendChild(c));
            } else if (k === 'style') {
                Object.keys(v).forEach(k => dom.style[k] = v[k]);
            } else if (k === 'classList') {
                v.forEach(c => dom.classList.add(c));
            } else {
                dom[k] = v;
                if (typeof v !== 'function') {
                    dom.setAttribute(k, v);
                }
            }
        }
        return dom;
    };

    return {
        opt: opt,
        promise: promise,
        http: http,
        mkDom: mkDom,
        range: (l, r) => new Array(r - l).fill(0).map((_, i) => l + i),
        deepCopy: val => JSON.parse(JSON.stringify(val)),
    };
});