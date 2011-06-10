type
   TMyObject = class
   public
      function getText : String; virtual;
   end;

function tMyObject.getText : String;
begin
   result:='We can ';
end;

type
   TSecond = class(tMyObject)
   public
     function getText:String; override;
   End;

function TSecond.getText:String;
begin
   result:=inherited getText + 'do it!';
end;

var o := TSecond.Create;
PrintLn(o.getText);
