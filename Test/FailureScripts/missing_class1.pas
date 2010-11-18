type
   TMyClass = class(TMissingClass)
   end;

var o : TObject;
(o as TMyClass).Create;
if o is TMissingClass then
   i:=TMissingClass.Create;

