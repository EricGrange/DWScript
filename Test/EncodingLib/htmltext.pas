PrintLn(HTMLTextEncoder.Encode(''));
PrintLn(HTMLTextEncoder.Encode('hello'));
PrintLn(HTMLTextEncoder.Encode('&hello'));
PrintLn(HTMLTextEncoder.Encode('hello>'));

var s := '<hello"world>here&';

var u := HTMLTextEncoder.Encode(s); 

PrintLn(u);

//var encoder := HTMLTextEncoder;

//PrintLn(encoder.Decode(u)); 
