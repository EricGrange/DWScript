unit ImplementInWrongUnit1;

interface

type
   TTest1 = class
      procedure World;
   end;

procedure World;

implementation

uses ImplementInWrongUnit2;

procedure Hello;
begin
   PrintLn('oops');
end;

procedure TTest2.Hello;
begin
   PrintLn('oops');
end;
