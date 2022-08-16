type C1 = class
  class procedure F1(valueStr: String); overload;
  begin
  end;
  class procedure F1(valueInt: Integer); overload;
  begin
  end;
  procedure F1(); overload;
  begin
  end;
end;

type C2 = class(C1)
  class procedure F2();
  begin
     F1();
  end;
end;