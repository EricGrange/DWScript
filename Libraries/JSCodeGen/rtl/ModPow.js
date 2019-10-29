function ModPow$(v, e, m) {
    if (m == 0) throw "ModPow division my zero";
    v %= m;
    if (e < 0) {
        e = -e;
        v = ModInv(v, m);
    }
    let r = 1n;
    while (e > 0) {
        if (v == 0) return 0n;
        if (e & 1n) r = (r * v) % m;
        e >>= 1n;
        v = (v * v) % m;
    }
    return r;
};
