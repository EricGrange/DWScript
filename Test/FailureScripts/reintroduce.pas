type
   TBase = class
      procedure Func; virtual;
   end;

type
   TChild = class (TBase)
      procedure Func; reintroduce;
   end;

type
   TChild2 = class (TBase)
      procedure Func;
   end;

type
   TSubChild = class (TChild)
      procedure Func; override;
   end;

type
   TSubChild2 = class (TChild)
      procedure Func; reintroduce;
   end;
