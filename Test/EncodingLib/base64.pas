var s := 'alpha'#9'omega';

var u := Base64Encoder.Encode(s); 

PrintLn(u);

var encoder := Base64Encoder;

PrintLn(encoder.Decode(u)); 
