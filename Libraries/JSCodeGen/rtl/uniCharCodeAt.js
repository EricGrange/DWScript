// inspired from 
// https://developer.mozilla.org/en/JavaScript/Reference/Global_Objects/String/charCodeAt
function $uniCharCodeAt(str, idx) {
    var c = str.charCodeAt(idx);
    if (0xD800 <= c && c <= 0xDBFF) { // High surrogate
        var lo = str.charCodeAt(idx+1);
        if (isNaN(lo)) return c;
        return ((c - 0xD800) * 0x400) + (lo - 0xDC00) + 0x10000;
    }
    if (0xDC00 <= c && c <= 0xDFFF) { // Low surrogate
        return -1;
    }
    return c;
}