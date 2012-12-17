type
  TClass1 = class
    class procedure Test1;
    class procedure Test2; static;
  end;
  
type
  TClassHelper1 = helper for TClass1
    class procedure ABC;
    class procedure DEF; static;
  end;

class procedure TClassHelper1.ABC;
begin
   Print('ABC: ');
end;

class procedure TClassHelper1.DEF;
begin
   PrintLn('DEF');
end;

class procedure TClass1.Test1;
begin
  Self.ABC; 
  Self.DEF; 
  ABC;
  DEF;
end;

class procedure TClass1.Test2;
begin
  ABC;
  DEF; 
end;

TClass1.Test1;
TClass1.Test2;