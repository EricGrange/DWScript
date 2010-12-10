
var i = Ord('A');
function MyString : String;
begin
   Result:=Chr(i);
   Inc(i);
end;

procedure DoPrint(nb : Integer; lazy str : String);
begin
   PrintLn('Print nb='+IntToStr(nb)+': '+str);
   if nb>1 then
      DoPrint(nb-1, str);
end;

procedure Local;
var
   v : String;
begin
   v:='World';
   DoPrint(3, v);
end;

type
   TMyObj = class
      Field : String;
      function IncField : String;
   end;

function TMyObj.IncField : String;
begin
   Field:=Field+Chr(Ord('a')+Length(Field));
   Result:=Field;
end;

DoPrint(3, 'hello');
Local;
DoPrint(4, MyString);

var o = TMyObj.Create;
DoPrint(3, o.IncField);
DoPrint(3, o.Field);
DoPrint(3, o.Field+o.IncField);

