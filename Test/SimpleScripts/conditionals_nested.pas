{$IFDEF TEST}
{$IFDEF ABCD}
PrintLn('Bug 1');
{$ENDIF}
{$ENDIF}

{$Define ABCD}

{$IFDEF TEST}
{$IFDEF ABCD}
PrintLn('Bug 2');
{$ENDIF}
{$ENDIF}

{$Define TEST}

{$IFDEF TEST}
{$IFDEF ABCD}
PrintLn('Hello world');
{$ENDIF}
{$ENDIF}

{$Undef ABCD}

{$IFDEF TEST}
{$IFDEF ABCD}
PrintLn('Bug 3');
{$ENDIF}
{$ENDIF}

