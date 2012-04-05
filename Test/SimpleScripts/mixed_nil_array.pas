type TTest = class end;

procedure PrintArray(a : array of TObject);
begin
   if a.Length=0 then
      PrintLn('Empty')
   else begin
      var i : Integer;
      for i:=0 to a.High do begin
         Print(i);
         Print(' : ');
         if a[i]=nil then
            PrintLn('nil')
         else PrintLn(a[i].ClassName);
      end;
   end;
end;

var a : array of TObject;
var o := TObject.Create;
var t := TTest.Create;

PrintArray(a);
a:=[nil];
PrintArray(a);
a:=[o, t];
PrintArray(a);
a:=[t, o];
PrintArray(a);
a:=[o, nil];
PrintArray(a);
a:=[nil, o];
PrintArray(a);
a:=[];
PrintArray(a);
