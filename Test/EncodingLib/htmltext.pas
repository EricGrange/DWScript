PrintLn(HTMLTextEncoder.Encode(''));
PrintLn(HTMLTextEncoder.Encode('hello'));
PrintLn(HTMLTextEncoder.Encode('&hello'));
PrintLn(HTMLTextEncoder.Encode('hello>'));
PrintLn(HTMLTextEncoder.Encode("'"#$a0));

var s := '<hello"world>here&';

var u := HTMLTextEncoder.Encode(s); 

PrintLn(u);

var encoder := HTMLTextEncoder;

PrintLn(encoder.Decode(u));

PrintLn(encoder.Decode('&amp;&lt;&#43;&gt;&apos;&quot;')); 

PrintLn(encoder.Decode('&num;&#x00023;&#35;'));

PrintLn(encoder.Decode('&&&bug;&;'));

PrintLn(encoder.Decode('<tag a="''" b=''"''>azerty</tag>'));

