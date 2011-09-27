type
   TSub = class end;

type 
   TRec = record
      x : Integer;
      y : String;
   end;

var ints : array of Integer;
var objs : array of TObject;
var subs : array of TSub;
var recs : array of TRec;

PrintLn(ints.indexOf('s'));
PrintLn(ints.IndexOf(2.5));
PrintLn(objs.IndexOf(3));
PrintLn(recs.IndexOf('s'));
PrintLn(recs.IndexOf(PrintLn('bug')));

PrintLn(objs.IndexOf(TSub.Create));
PrintLn(subs.IndexOf(TSub.Create));
PrintLn(subs.IndexOf(TObject.Create));

PrintLn('s' in ints);
PrintLn(2.5 in ints);
PrintLn(2.5 in objs);
PrintLn(3 in recs);
PrintLn(PrintLn('bug') in recs);

