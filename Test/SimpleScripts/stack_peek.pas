type TStringDynArray = array of string;
var s : TStringDynArray;

procedure DoPeek(s : TStringDynArray);
begin
   try 
      PrintLn(s.Peek);
   except
      on E: Exception do PrintLn(E.Message);
   end;
end;

DoPeek(s);
s:=['hello', 'world'];
DoPeek(s);
s.Swap(0, 1);
DoPeek(s);
s.Delete(0);
DoPeek(s);
s.Clear;
DoPeek(s);