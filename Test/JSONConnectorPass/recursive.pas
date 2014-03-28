function SubTest(path : String ; idx, lvl : Integer) : JSONVariant;
var
   i : Integer;
   a : JSONVariant;
begin
   Result:=JSON.NewObject;
   Result.Code:=path+IntToStr(idx);
   a:=JSON.NewArray;
   if lvl<=1 then begin
      for i:=0 to 1 do begin
         a[i]:=SubTest(Result.Code, i, lvl+1);
      end;
   end;
   Result.Sub:=a;
end;

function TestJSON : String;
begin
   var a:=JSON.NewArray;
   a[0]:=SubTest('', 0, 0);
   Result:=JSON.Stringify(a);
end;
PrintLn(TestJSON);