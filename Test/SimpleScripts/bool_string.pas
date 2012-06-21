var b : Boolean;
var s : String;

PrintLn(BoolToStr(b));
PrintLn(StrToBool(BoolToStr(b)));
b:=not b;
PrintLn(BoolToStr(b));
PrintLn(StrToBool(BoolToStr(b)));

var a := ['y', 'Y', 'n', 'N', 
          'yes', 'Yes',
          'no', 'No',
          't', 'T', 'f', 'F',
          'true', 'True',
          'false', 'False',
          'dummy'];
           
var i : Integer;
for s in a do begin
   Print(s);
   Print(' : ');
   PrintLn(StrToBool(s));
end;


