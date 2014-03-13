var s := 'éric';

var u := UTF8Encoder.Encode(s); 

PrintLn(u);

var encoder := UTF8Encoder;

PrintLn(encoder.Decode(u)); 
