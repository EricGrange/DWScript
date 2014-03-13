var s := 'url encoded/+'#1'that é!';

var u := URLEncodedEncoder.Encode(s); 

PrintLn(u);

var encoder := URLEncodedEncoder;

PrintLn(encoder.Decode(u)); 
