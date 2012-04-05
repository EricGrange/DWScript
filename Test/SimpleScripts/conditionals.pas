{$IFDEF TEST}
PrintLn('Bug 1');
{$ENDIF junk}

{$IFNDEF TEST}
PrintLn('Hello');
{$ENDIF}

{$Define TEST}

{$IFDEF TEST}
PrintLn('World');
{$ENDIF}

{$IFNDEF TEST}
PrintLn('Bug 2');
{$ENDIF}

{$undef TEST}

{$IFDEF TEST}
PrintLn('Bug 3');
{$ENDIF}

{$IFNDEF TEST}
PrintLn('!')
{$ENDIF}

