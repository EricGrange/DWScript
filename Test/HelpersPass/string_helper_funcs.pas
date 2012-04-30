type
   TStringHelper = helper for String
      class var n : Integer;
      class function CountIt : String;
      procedure Print;
      procedure PrintCount;
   end;

class function TStringHelper.CountIt : String;
begin
   Inc(n);
   Result:=IntToStr(n);
end;

procedure TStringHelper.Print;
begin
   PrintLn(Self);
end;

procedure TStringHelper.PrintCount;
begin
   Print;
   CountIt.Print;
end;
   
var s : String = "Hello";

s.Print;
IntToStr(123).Print;

s.CountIt.Print;
IntToStr(123).CountIt.Print;

s.PrintCount;
IntToStr(123).PrintCount;

TStringHelper.CountIt.Print;
