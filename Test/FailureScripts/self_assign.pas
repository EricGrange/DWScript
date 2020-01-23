var a := 0;
a := a;

procedure Test(a : Integer);
begin
   a := a;
end;

type
   TTest = class
      F : Integer;
      procedure Test1; begin F := F; end;
      procedure Test2(f : Integer); begin f := f; end;
    end; 
