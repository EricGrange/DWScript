var s := 'url encoded/+'#1'that é!';

var u := URLEncodedEncoder.Encode(s); 

PrintLn(u);

var encoder := URLEncodedEncoder;

PrintLn(encoder.Decode(''));
PrintLn(encoder.Decode(u));

PrintLn(encoder.Decode('%c3%a9%5d'));
PrintLn(encoder.Decode('%z'));
PrintLn(encoder.Decode('%zz')=#$fffd);
PrintLn(encoder.Decode('%1z')=#$fffd);
