type
   TList<T> = class
      private
         FData : array of T;
         FCount : Integer;
         
      public
         function Add(v : T) : Integer;
         begin
            FData.Add(v);
            FCount += 1;
            Result := FCount;
         end;

         procedure Clear;
         begin
            FData.Clear;
            FCount := 0;
         end;

         property Count : Integer read FCount;
         property Length : Integer read (FData.Length);
   end;

var s := new TList<String>;
PrintLn(s.Count);
PrintLn(s.Add('hello'));
PrintLn(s.Add('world'));
PrintLn(s.Count.ToString + ',' + s.Length.ToString);
s.Clear;

var i := new TList<Integer>;
PrintLn(i.Add(123));
PrintLn(i.Add(456));

type TRec = record a,b : Integer end;
var r := new TList<TRec>;
var r1 : TRec;
PrintLn(r.Add(r1));
PrintLn(r.Add(r1));

PrintLn(s.Count.ToString + ',' + s.Length.ToString);
PrintLn(i.Count.ToString + ',' + i.Length.ToString);
PrintLn(r.Count.ToString + ',' + r.Length.ToString);