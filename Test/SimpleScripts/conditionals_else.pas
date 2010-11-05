{$IFDEF TEST}
PrintLn('Bug 1');
{$ELSE}
PrintLn('Hello');
{$ENDIF}

{$Define test}

{$ifdef TEST}
PrintLn('World');
{$else}
PrintLn('Bug 2');
{$endif}

