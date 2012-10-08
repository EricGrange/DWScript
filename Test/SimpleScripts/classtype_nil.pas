var c : TClass;

if c=nil then 
   PrintLn('nil default');

c := TObject.ClassType;

if c<>nil then 
   PrintLn(c.ClassName);

c:=nil;

if c<>nil then 
   PrintLn('bug 3')
else PrintLn('set nil');