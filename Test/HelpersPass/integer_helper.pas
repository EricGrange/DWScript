Type

 TIntegerHelper = Helper For Integer

 Function ToString : String;
 Begin
   if Abs(Self)>1000 then
      Result:='big'
   else Result := IntToStr(Self);
 End;

 End;

PrintLn(StrToInt('5').ToString);
PrintLn(High(Integer).ToString); 