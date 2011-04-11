type
   TMyClass = class
      constructor Create;
      constructor VirtualCreate; virtual;
      method Dummy;
   end;

constructor TMyClass.Create;
begin
   VirtualCreate;
   Create;
end;

constructor TMyClass.VirtualCreate;
begin
   Create;
   VirtualCreate;
end;

method TMyClass.Dummy;
begin
   Create;
   VirtualCreate;
end;

var o : TMyClass;

o.Create;
o.VirtualCreate;