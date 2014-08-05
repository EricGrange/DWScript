type
   TMyClass = class
   public
      constructor Create(x: String);
	  begin
		Print(ClassName);
		PrintLn(x);
	  end;
   end;
   
type
   TAlias = TMyClass;

var myObj := new TAlias(' hello');
myObj := TAlias.Create(' world');
myObj := new (TAlias)(' !');