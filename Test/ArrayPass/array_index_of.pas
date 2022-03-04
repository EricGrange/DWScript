var ints : array of Integer;

ints.Add(1);
ints.Add(2);

PrintLn('Integers');

PrintLn(ints.IndexOf(0));
PrintLn(ints.IndexOf(1));
PrintLn(ints.IndexOf(2));
PrintLn(ints.IndexOf(1,1));
PrintLn(ints.IndexOf(2,1));

type 
   TRec = record
      x : Integer;
      y : String;
   end;
var recs : array of TRec;

const r1 : TRec = (x: 1; y: 'one');
const r2 : TRec = (x: 2; y: 'two');
const r3 : TRec = (x: 2; y: 'twobis');

PrintLn('Records');

recs.Add(r1);
recs.Add(r2);

PrintLn(recs.IndexOf(r1));
PrintLn(recs.IndexOf(r2));
PrintLn(recs.IndexOf(r3));

var objs : array of TObject;

var o := new TObject;

objs.Add(TObject.Create);
objs.Add(o);

PrintLn('Objects');

PrintLn(objs.indexof(TObject.Create));
PrintLn(objs.indexof(o));