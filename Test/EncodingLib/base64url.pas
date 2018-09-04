
var s := '!>>';

var u := Base64URIEncoder.Encode(s);
PrintLn(u);
PrintLn(Base64URIEncoder.Decode(u));

s := '!>';
u := Base64URIEncoder.Encode(s);
PrintLn(u);
PrintLn(Base64URIEncoder.Decode(u));

s := 'a';
u := Base64URIEncoder.Encode(s);
PrintLn(u);
PrintLn(Base64URIEncoder.Decode(u));

var n := 0;
for var i := 1 to 9 do begin
   n := n*10 + i;
   u := Base64URIEncoder.Encode(n.ToString);
   PrintLn(u + ' -> ' + Base64URIEncoder.Decode(u));
end;

