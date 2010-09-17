type
   TTest = class
      FInteger : Integer;
      FString : String;
      FFloat : Float;
      constructor CreateInt(i : Integer = 123);
      constructor CreateStr(s : String = 'ABC');
      constructor CreateFloat(f : Float = 12.3);

      procedure SetInt(i : Integer = 456);
      procedure SetStr(s : String = 'DEF');
      procedure SetFloat(f : Float = 4.56);

      procedure PrintOut;
   end;

constructor TTest.CreateInt(i : Integer = 123);
begin
   FInteger:=i;
end;

constructor TTest.CreateStr(s : String = 'ABC');
begin
   FString:=s;
end;

constructor TTest.CreateFloat(f : Float = 12.3);
begin
   FFloat:=f;
end;

procedure TTest.SetInt(i : Integer = 456);
begin
   FInteger:=i;
end;

procedure TTest.SetStr(s : String = 'DEF');
begin
   FString:=s;
end;

procedure TTest.SetFloat(f : Float = 4.56);
begin
   FFloat:=f;
end;

procedure TTest.PrintOut;
begin
   PrintLn(IntToStr(FInteger)+','+FString+','+FloatToStr(FFloat));
end;

procedure PrintInt(i : Integer = 789);
begin
   PrintLn(IntToStr(i));
end;

procedure PrintStr(s : String = 'IJK');
begin
   PrintLn(s);
end;

procedure PrintFloat(f : Float = 78.9);
begin
   PrintLn(FloatToStr(f));
end;

var t : TTest;

TTest.CreateInt(1).PrintOut;
TTest.CreateInt.PrintOut;
TTest.CreateStr('zzz').PrintOut;
TTest.CreateStr.PrintOut;
TTest.CreateFloat(3.14).PrintOut;
TTest.CreateFloat.PrintOut;

PrintLn('');
PrintLn('---');
PrintLn('');

t:=TTest.Create;
t.PrintOut;

t.SetInt;
t.PrintOut;
t.SetInt(2);
t.PrintOut;

t.SetStr;
t.PrintOut;
t.SetStr('aaa');
t.PrintOut;

t.SetFloat;
t.PrintOut;
t.SetFloat(2.5);
t.PrintOut;

PrintLn('');
PrintLn('---');
PrintLn('');

PrintInt;
PrintInt(1);
PrintStr;
PrintStr('bbb');
PrintFloat;
PrintFloat(1.5);

