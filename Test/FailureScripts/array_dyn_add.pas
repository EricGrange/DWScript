var ints : array of Integer;
var stints : array [0..1] of Integer;

ints.Add(TObject.Create);
ints.Add('a');
ints.Add(1);
ints.Add(1.5);
ints.Add(False);
ints.Add(stints);  
ints.Add(nil);

var objs : array of TObject;

objs.Add(TObject.Create);
objs.Add('a');
objs.Add(1);
objs.Add(1.5);
objs.Add(False);
objs.Add(ints);
objs.Add(nil);
objs.Add(PrintLn('bug'));