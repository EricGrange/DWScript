type
   TMyClass = class
      FField : Integer;
      procedure PrintMe; virtual; begin PrintLn(FField); end;
   end;

procedure TMyClass.PrintMe;
begin
   PrintLn(FField);
end;

type
   TMySubClass = class (TMyClass)
      procedure PrintMe; override;
      begin
      Print(FField);
      Print(FField2);
      end;
      FField2 : Integer;
   end;

