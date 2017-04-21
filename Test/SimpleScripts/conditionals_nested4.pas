{$IF Declared('Test')}
  {$IF Test>=100}
     PrintLn('bug1');
  {$ENDIF}
  {$IF Test>=100}
     PrintLn('bug2');
  {£ELSE}
     PrintLn('bug3');
  {$ENDIF}
{$ENDIF}

const Test = 101;

{$IF Declared('Test')}
  {$IF Test>=100}
     PrintLn('ok1');
  {$ENDIF}
  {$IF Test>=100}
     PrintLn('ok2');
  {$ELSE}
     PrintLn('bug4');
  {$ENDIF}
  {$IF Test<100}
     PrintLn('bug5');
  {$ELSE}
     PrintLn('ok3');
  {$ENDIF}
{$ENDIF}
