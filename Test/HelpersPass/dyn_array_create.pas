Type TStrings = Array Of String;

Type

 TStringsHelper = Helper For TStrings
 
  Class Function Create(Const AParams : Array Of String) : TStrings; Static;
  Begin
  
   Result := AParams;
  
  End; 
  
  class var iterator : Integer;
  class function Iterate(n : Integer) : TStrings;
  begin
     while n>0 do begin
       Result.Add(IntToStr(iterator));
       iterator+=1;
       Dec(n);
     end;
  end;
 
 End;

var s : String;
var sa := TStrings.Create(['1', '2']); 

For s In sa Do
 PrintLn(s);
 
For s in TStrings.Create(['3', '4']) Do
 PrintLn(s);

For s in TStrings.Iterate(1) Do
  PrintLn(s);

For s in TStrings.Iterate(2) Do
  PrintLn(s);
  
sa := TStrings.Iterate(2);
For s in sa Do
  PrintLn(s);
  