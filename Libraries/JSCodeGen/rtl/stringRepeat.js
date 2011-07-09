function stringRepeat(s, n) {
    if (n<1) return '';
    var r = '';
    while (n > 0) {
        if (n & 1) r += s;
        n >>= 1, s += s;
    };
    return r;
};
