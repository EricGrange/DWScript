type
   TMyRec = record
      private
         FHidden : String;
      public
         Pub : String;
      published   
         Doh : String;
   end;
 
const cr : TMyRec = (
   FHidden : 'hello';
   Pub : 'World';
   Doh : 'Duh'); 
   
var r := cr;
 
r.FHidden:='a';
r.Pub:='a';
r.Doh:='a';

PrintLn(r.FHidden);
PrintLn(r.Pub);
PrintLn(r.Doh);