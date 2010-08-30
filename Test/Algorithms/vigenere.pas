{
   Inspired from http://delphi.about.com/b/2010/08/30/vigenere-cipher-algorithm-delphi-implementation-comments-by-alan-lloyd.htm
   which looked arcane enough to be worthy of a unit test

   This implementation is rather fragile, don't reuse ;)

}
function Vigenere(src, key : String; encrypt : boolean) : String;
begin
   const cOrdMinChar : integer = Ord('A');
   const cOrdMaxChar : integer = Ord('Z');
   const cCharRangeCount = cOrdMaxChar - cOrdMinChar + 1;

   var i, j, keyLen, keyInc, srcOrd, cryptOrd : integer;
   var srcA : String;


   keyLen := Length(key);
   SetLength(srcA, Length(src));
   if encrypt then begin
      // transfer only included characters to srcA for encryption
      j := 1;
      for i := 1 to Length(src) do begin
         case src[i] of
            'A'..'Z': begin
               srcA[j] := src[i];
               j:=j+1;
            end;
         end;
      end;
      SetLength(srcA, j - 1);
   end;

   if encrypt then begin
      // encrypt to Result
      SetLength(Result, Length(srcA));
      for i := 1 to Length(srcA) do begin
         srcOrd := Ord(srcA[i]) - cOrdMinChar;
         keyInc := Ord(key[(i-1) mod keyLen + 1]) - cOrdMinChar;
         cryptOrd := ((srcOrd + keyInc) mod cCharRangeCount) + cOrdMinChar;
         Result[i] := Chr(cryptOrd);
      end;
   end else begin
      // Decrypt to Result
      SetLength(Result, Length(src));
      for i := 1 to Length(src) do begin
         srcOrd := Ord(src[i]) - cOrdMinChar;
         keyInc := Ord(key[(i-1) mod keyLen + 1]) - cOrdMinChar;
         cryptOrd := ((srcOrd - keyInc + cCharRangeCount) mod cCharRangeCount) + cOrdMinChar;
         // keyInc may be larger than srcOrd
         Result[i] := Chr(cryptOrd);
      end;
   end;
end;

const key : String = 'MYKEY';
var buf : String;

buf:=Vigenere('HELLO WORLD', key, True);

PrintLn(buf);

buf:=Vigenere(buf, key, False);

PrintLn(buf);
