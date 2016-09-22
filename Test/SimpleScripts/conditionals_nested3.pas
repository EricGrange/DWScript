{$ifdef CompilerVersion}
  {$if CompilerVersion = 1}
  PrintLn(1);
  {$endif}
{$endif}

{$ifdef CompilerVersion}
  {$if CompilerVersion <> 1}
  PrintLn(2);
  {$endif}
{$endif}

{$ifndef CompilerVersion}
  {$if CompilerVersion = 1}
  PrintLn(3);
  {$endif}
{$endif}

{$ifndef CompilerVersion}
  {$if CompilerVersion <> 1}
  PrintLn(4);
  {$endif}
{$endif}