var &begin : Integer;

type &end = class(TObject);

type &procedure = class(&end);

procedure &shl(&then : String);
begin
   PrintLn(&then+IntToStr(&begin));
   &begin:=&begin+1;
end;

&shl(&end.ClassName);
&shl(&procedure.ClassName);
PrintLn(&begin);