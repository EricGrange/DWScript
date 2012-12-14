type
   TMyClass = class
      class procedure Test1; safecall; begin end;
      procedure Test2; stdcall; begin end;
      method Test3; virtual; cdecl; begin end;
   end;
   
procedure Test4; register;
begin
end;

procedure Test5; overload; pascal;
begin
end;
