function ModInv(v, n) {
    let t = 0n, nT = 1n, r = n, nR = v < 0 ? -v : v;
    while (nR != 0) {
        let q = r / nR, pT = t, pR = r;
        t = nT;
        r = nR;
        nT = pT - q*nT;
        nR = pR - q*nR;
    }
    if (r != 1 && r != -1) return 0n;
    if (t < 0) t += n;
    return v < 0 ? n - t : t;
}
