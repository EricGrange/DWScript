PrintLn(HTMLAttributeEncoder.Encode(''));
PrintLn(HTMLAttributeEncoder.Encode('hello'));
PrintLn(HTMLAttributeEncoder.Encode('&/?'));

var s := '%%aaaaaaaaa';
var e := HTMLAttributeEncoder.Encode(s); 
PrintLn(e);
PrintLn(HTMLAttributeEncoder.Decode(e));
