unit HelloWorld2;

interface

type
   TMyClass1 = class end;
   TMyClass2 = class end;
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