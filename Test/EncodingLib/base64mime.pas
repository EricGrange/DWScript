PrintLn(Base64Encoder.EncodeMIME('') = '');

var s := StringOfString('hello', 5);
var u := Base64Encoder.EncodeMIME(s); 
PrintLn(u);
PrintLn(Base64Encoder.Decode(u) = s);

s := StringOfString('hello', 50);
u := Base64Encoder.EncodeMIME(s); 
PrintLn(u);
PrintLn(Base64Encoder.Decode(u) = s);

