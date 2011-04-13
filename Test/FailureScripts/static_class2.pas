type
   TDummy = class 
   end;

type
   TDummyStatic = class static (TDummy)
   end;

type
   TStatic = class static (TObject)
   end;

type 
   TSub = class static (TStatic)
   end;
