type
   TBase = class
      Field : Integer;
      procedure Event(s : String); virtual;
      begin
         PrintLn('Base '+s+' '+IntToStr(Field)+' '+ClassName);
      end;
   end;
   
type
   TChild = class(TBase)      
      procedure Event(s : String); override;
      begin
         PrintLn('Child '+s+' '+IntToStr(Field)+' '+ClassName);
      end;
   end;

type
   TSubChild = class(TChild) 
   end;
   
var e : procedure (s:String);

var b := new TBase;
var c := new TChild;
var s := new TSubChild;
b.Field:=1;
c.Field:=2;
s.Field:=3;

e:=b.Event;
e('a');

e:=c.Event;
e('b');

e:=s.Event;
e('c');

e:=TBase(c).Event;
e('d');

e:=TBase(s).Event;
e('f');

e:=TChild(s).Event;
e('g');


