Type

 THelper = Helper For array of Integer
 
  Function Length : Integer;
   
 End;
 
function THelper.Length : Integer;
begin
   Print('I saw ');
   Result:=inherited Length;
end; 
 
var a : array of Integer;

PrintLn(a.Length);

a.Add(1);

PrintLn(a.Length);

