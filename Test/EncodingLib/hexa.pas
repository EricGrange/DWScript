var s := 'alpha'#9'omega';

var u := HexadecimalEncoder.Encode(s); 

PrintLn(u);

var encoder := HexadecimalEncoder;

PrintLn(encoder.Decode(u)); 

s := '';
for var i := 0 to 255 do
	s := Chr(i);

if encoder.Decode(encoder.Encode(s))<>s then 
	PrintLn('bug');
