type
   TVarProc = procedure (sender : TObject; var s : String);

var proc : TVarProc;

procedure Stuff(sender : TObject; var s : String);
begin
   s := sender.ClassName;
end;

proc := Stuff;

var s : String := 'hello';
var obj : TObject := TObject.Create;

PrintLn(s);
proc(obj, s);
PrintLn(s);

type
   TMyClass = class
      procedure Stuff(sender : TObject; var s : String);
      begin
         s := sender.ClassName + '!';
      end;
   end;
   
proc := TMyClass.Create.Stuff;
proc(obj, s);
PrintLn(s);
proc(TMyClass.Create, s);
PrintLn(s);