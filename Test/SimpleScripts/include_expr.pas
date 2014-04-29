PrintLn({$I %FILE%}+','+{$INCLUDE %LINE%});
PrintLn({$I %FILE%}+','+{$INCLUDE %LINE%});
PrintLn({$I %MAINFILE%}+','+IntToStr(1000+{$INCLUDE %LINENUM%}));

{$INCLUDE 'include_expr.inc'}

if FormatDateTime('yyyy-mm-dd', Now)<>{$I %DATE%} then
   PrintLn('Date mismatch');

procedure MyFunc;
begin
   PrintLn({$I %FUNCTION%});
end;

type TMyClass = class
   method MyMethod;
end;

method TMyClass.MyMethod;
begin
   PrintLn({$I %FUNCTION%});
end;

PrintLn({$I %FUNCTION%});
MyFunc;
TMyClass.Create.MyMethod;

