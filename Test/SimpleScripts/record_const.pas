type TBase = record
   Field : String;
   private const cPrivate = 1;
   public class const cPublic = 3;
   procedure PrintAll;
   class procedure ClassPrintAll;
end;
  
procedure TBase.PrintAll;
begin
   PrintLn('From base');
   PrintLn(cPrivate);
   PrintLn(cPublic);
end;

class procedure TBase.ClassPrintAll;
begin
   PrintLn('From class proc');
   PrintLn(cPrivate);
   PrintLn(cPublic);
end;

var o : TBase;

PrintLn('From Outside');
PrintLn(TBase.cPublic);
PrintLn(o.cPublic);

TBase.ClassPrintAll;
o.PrintAll;






