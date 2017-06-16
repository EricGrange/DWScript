var s := 'alpha'#9'omega';

var u := Base64Encoder.Encode(s); 

PrintLn(u);

var encoder := Base64Encoder;

PrintLn(encoder.Decode(u)); 

PrintLn(Base64Encoder.Decode('aGVs'#13'bG8='#10));
