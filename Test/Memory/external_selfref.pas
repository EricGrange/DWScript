type
   TMyObj = class(TExposedClass)
      Field : TMyObj;
   end;

var o1, o2, o3 : TMyObj;

o1:=TMyObj.Create;

o2:=TMyObj.Create;
o2.Field:=o2;

o3:=TMyObj.Create;
o3.Field:=TMyObj.Create;
