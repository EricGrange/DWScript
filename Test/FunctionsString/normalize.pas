var s := 'ériç';

var nfkd := s.Normalize('NFKD');

PrintLn(nfkd.Length);
for var i := 1 to High(nfkd) do
	Print(Ord(nfkd[i]).ToHexString(4).ToUpper);
PrintLn('');

var nfc := NormalizeString(nfkd);

PrintLn(nfc.Length);
for var i := 1 to High(nfc) do
	Print(Ord(nfc[i]).ToHexString(4).ToUpper);
PrintLn('');

if nfc <> s then PrintLn('bug');
