var k := TRSAKey.Generate(512);

var kExport := JSON.Parse(k.ExportJSON);
PrintLn(kExport.size);
PrintLn(Length(kExport.modulus)*4);
for var n := 0 to kExport.High() do
   PrintLn(kExport.ElementName(n));

var msg := 'hello world';
var msgHash := HashSHA256.HashData(msg);

PrintLn('msg hash ' + msgHash);

var sig := k.SignHash('SHA256', msgHash);

PrintLn(k.VerifyHash('SHA256', msgHash, sig));
PrintLn(k.VerifyHash('SHA256', HashSHA256.HashData(msg+'j'), sig));

var j := JSON.Parse(k.ExportJSON);
j.Delete('prime1');
j.Delete('prime2');
kExport := JSON.Parse(j.ToString());
for var n := 0 to kExport.High() do
   PrintLn(kExport.ElementName(n));

var k2 := TRSAKey.ImportJSON(j);
PrintLn(k2.VerifyHash('SHA256', msgHash, sig));
PrintLn(k2.VerifyHash('SHA256', HashSHA256.HashData(msg+'j'), sig));

try
   k2.SignHash('SHA256', msgHash);
except
   on E: Exception do
      PrintLn(E.Message);
end;

var k3 := TRSAKey.ImportJSON(k.ExportJSON);
PrintLn(k3.VerifyHash('SHA256', msgHash, sig));

var reSig := k3.SignHash('SHA256', msgHash);
PrintLn(sig = reSig);
