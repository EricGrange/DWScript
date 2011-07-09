Type TMyRecord = Record
  mrId: Integer;
  mrObj: TObject;
End;

var mRec: TMyRecord;

if not Assigned(mRec.mrObj) then PrintLn('not assigned');
if Assigned(mRec.mrObj) then PrintLn('assigned');
PrintLn(mRec.mrId);

mRec.mrObj:=TObject.Create;
mRec.mrId:=12;

PrintLn(mRec.mrObj.ClassName);
PrintLn(mRec.mrId);

mRec.mrObj:=NIL;
mRec.mrId:=0;

if mRec.mrObj<>nil then PrintLn('assigned');
if mRec.mrObj=nil then PrintLn('not assigned');
PrintLn(mRec.mrId);

