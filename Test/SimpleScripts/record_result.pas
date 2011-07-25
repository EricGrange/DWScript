type
   TMonRecord = record
      Nom : string;
      Blabla : string
   end;
 
var i : Integer; 
 
function GetUnRecord() : TMonRecord;
begin
   result.Nom := 'azerty';
   result.Blabla := IntToStr(i);
   i:=i+1;
end;
 
var toto : TMonRecord;
 
toto := GetUnRecord();
PrintLn(toto.Nom);
PrintLn(toto.Blabla);

toto := GetUnRecord;
PrintLn(toto.Nom);
PrintLn(toto.Blabla);

PrintLn(GetUnRecord.Nom);
PrintLn(GetUnRecord.Blabla);