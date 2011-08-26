type 
   IMyIntf = interface
      function GetHello : String;
      procedure SetHello(v1, v2 : String);
      
      property Hello : String read GetHello;
      property World[v : String] : String write SetHello; default;
   end;
   
type
   TImplem = class (TObject, IMyIntf)
      Field : String;
      function GetHello : String; begin Result:=Field; end;
      procedure SetHello(v1, v2 : String);
      begin 
         Field:=v1+' '+v2;
      end;
   end;
   
var i := TImplem.Create as IMyIntf;

i.World['Hello']:='World';
PrintLn(i.Hello);

i['Bye']:='Test';
PrintLn(i.Hello);
  
