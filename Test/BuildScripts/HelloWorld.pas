unit HelloWorld;

{$DEFINE HELLO}

procedure PrintHelloWorld;
begin
   PrintLn('Hello World!');
end;

{$IFDEF HELLO2}
{$WARNING 'define HELLO2 is set!'}
{$ENDIF}
