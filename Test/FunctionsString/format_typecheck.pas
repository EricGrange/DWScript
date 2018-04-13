var ID : String := '22';
var temp : String;

try
   temp := Format('%d %d', [ID, ID]);
except
   on E: Exception do 
      PrintLn(E.ClassName+': '+E.Message.Before("'")+E.Message.After("' "));
end;
