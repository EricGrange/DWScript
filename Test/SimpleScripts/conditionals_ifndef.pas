function Test({$ifdef TEST}{$endif}s : String);


{$DEFINE TEST}
type
   TDummy = class
  {$IFNDEF TEST}
  bug here
  {$ENDIF}
  end;
  
PrintLn(TDummy.ClassName);