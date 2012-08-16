type
   TMyAttr = class(TCustomAttribute)
   end;
   
type
   [TMyAttr]
   TTest1 = class end;

type
   [TMyAttr()]
   TTest2 = class end;

type
   TStrings = array of string;   
   
type
   [TStrings[2]]
   TTest3 = class end;
   
type
   [TObject]
   TTest4 = class end;
   
type
   [bug]
   TTest5 = class end;
  