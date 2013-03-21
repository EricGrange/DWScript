type
   TMyClass = class
      class var F : Integer;
      class property P : Integer read F write F;
	  class property PA[i : Integer] : Integer read (F+i) write F;
   end;

TMyClass.P := TMyClass.P + 1;
TMyClass.P += 1;

TMyClass.PA[1] := TMyClass.PA[1] + 1;
TMyClass.PA[1] += 1;