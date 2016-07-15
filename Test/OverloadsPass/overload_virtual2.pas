
type
   TObj1 = class
      constructor Create; overload; virtual;
      begin
         PrintLn('TObj1.Create');
         inherited Create;
      end;
      constructor Create(p: Integer); overload; virtual;
      begin
         PrintLn('TObj1.Create('+p.ToString+')');
         Create;
      end;
   end;

type
   TObj2 = class (TObj1)
      constructor Create(p: Integer); overload; override;
      begin
         PrintLn('TObj2.Create('+p.ToString+')');
         inherited Create(p);
      end;
   end;

type
   TObj3 = class (TObj2)
      constructor Create; overload; override;
      begin
         PrintLn('TObj3.Create');
         inherited Create;
      end;
   end;

var o := TObj3.Create(1);
PrintLn(o.ClassName);