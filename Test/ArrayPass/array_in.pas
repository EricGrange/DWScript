var ints : array of Integer;

ints.Add(1);
ints.Add(2);

PrintLn('Integers');

PrintLn(0 in ints);
PrintLn(1 in ints);
PrintLn(2 in ints);
PrintLn(0 not in ints);
PrintLn(1 not in ints);
PrintLn(2 not in ints);

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

PrintLn(r1 in recs);
PrintLn(r2 in recs);
PrintLn(r3 in recs);

var objs : array of TObject;

var o := new TObject;

objs.Add(TObject.Create);
objs.Add(o);

PrintLn('Objects');

PrintLn(TObject.Create in objs);
PrintLn(o in objs);