unit const_inline;

interface

procedure Test(i : Integer);

implementation

const 
   one = 1;
   two = 2;
   
procedure Test(i : Integer);
const 
   beta = 'b';
   gamma = 'g';
var
   s : String;
begin
   const alpha = 'a';
   s := 'test ';
   case i of
      one, two : 
         if i=one then
            s+=alpha
         else s+=beta;
      one+two :
         s+=gamma;
   end;
   PrintLn(s);
end;

end.