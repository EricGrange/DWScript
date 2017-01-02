
var pub1, priv1 : String;
var pub2, priv2 : String;

ECCsecp256r1.MakeKey(pub1, priv1);
ECCsecp256r1.MakeKey(pub2, priv2);

if (pub1=pub2) or (priv1=priv2) then PrintLn('MakeKey bug');

var sec1 := ECCsecp256r1.ECDHSharedSecret(pub1, priv2);
var sec2 := ECCsecp256r1.ECDHSharedSecret(pub2, priv1);

PrintLn(sec1 = sec2);

if sec1<>ECCsecp256r1.ECDHSharedSecret(pub1, priv2) then PrintLn('Secret bug 2');

var hash := 'cda90d38a8c1131fe1a587d81945f711bc2727d15b00f0336e3d604550c6ad82';

var sig1 := ECCsecp256r1.ECDSASign(priv1, hash);
var sig2 := ECCsecp256r1.ECDSASign(priv2, hash);

if sig1=sig2 then PrintLn('Sign bug');

PrintLn(ECCsecp256r1.ECDSAVerify(pub1, hash, sig1));
PrintLn(ECCsecp256r1.ECDSAVerify(pub2, hash, sig1));
PrintLn(ECCsecp256r1.ECDSAVerify(pub1, hash, sig2));
PrintLn(ECCsecp256r1.ECDSAVerify(pub2, hash, sig2));


