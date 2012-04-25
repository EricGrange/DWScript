unit HelloWorld2;

interface

{$DEFINE HELLO2}

type
   TMyClass1 = class end;
   TMyClass2 = class end;
   
type   
   TMyAlias = Integer;

procedure PrintHelloWorld2a; forward;

procedure PrintHelloWorld2b;

implementation

procedure PrintHelloWorld2a;
begin
   PrintLn('Hello World2a!');
end;

procedure PrintHelloWorld2b;
begin
   PrintLn('Hello World2b!');
end;

{$IFDEF HELLO}
{$WARNING 'define HELLO is set!'}
{$ENDIF}
