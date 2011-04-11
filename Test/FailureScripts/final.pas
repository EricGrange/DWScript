type
   TClassA = class
      procedure Virt; virtual;
      procedure Stat;
   end;
type
   TClassB1 = class(TClassA)
      procedure Virt; override;
      procedure Stat; final;
   end;
type
   TClassB2 = class(TClassA)
      procedure Virt; final;
      procedure Stat; final;
   end;
type
   TClassC = class(TClassB1)
      procedure Virt; override; final;
   end;
type
   TClassD1 = class(TClassC)
      procedure Virt; override;
   end;
type
   TClassD2 = class(TClassC)
   end;
type
   TClassE = class(TClassC)
      procedure Virt; override;
   end;

{$FATAL "done"} // prevents litany of missing implementations