unit ImplementInWrongUnit2;

interface

type
   TTest2 = class
      procedure Hello;
   end;
   
procedure Hello;

implementation

uses ImplementInWrongUnit1;

procedure World;
begin
   PrintLn('oops');
end;

procedure TTest1.World;
begin
   PrintLn('oops');
end;
