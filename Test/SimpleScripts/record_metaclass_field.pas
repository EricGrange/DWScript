type TMy = class end;

type TMyRecord = record
  c1 : TClass;
  c2 : class of TMy;
end;

var r : TMyRecord;

if Assigned(r.c1) then PrintLn('bug1');
if r.c2<>nil then PrintLn('bug2');

r.c1 := TObject;
r.c2 := TMy;

var r2 := r;

PrintLn(r2.c1.ClassName);
PrintLn(r2.c2.ClassName);
