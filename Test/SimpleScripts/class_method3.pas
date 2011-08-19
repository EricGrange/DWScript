type
   TBase = class
      class method Test; virtual;
      class method Test2;
      class method CallVirtual; virtual;
      class method Call;
   end;
   
type
   TChild = class(TBase)
      class method CallVirtual; override;
   end;

class method TBase.Test;
begin
   PrintLn('test '+ClassName);
end;

class method TBase.Test2;
begin
   PrintLn('test2 '+ClassName);
end;

class method TBase.CallVirtual;
begin
   Test;
   Test2;
end;

class method TBase.Call;
begin
   Test;
   Test2;
end;

class method TChild.CallVirtual;
begin
   PrintLn('overridden');
   inherited CallVirtual;
end;

var c : TChild := new TChild;
   
c.Test;
TChild.Test;
c.Test2;
TChild.Test2;

c.Call;
c.CallVirtual;

TChild.Call;
TChild.CallVirtual;
