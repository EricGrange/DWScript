type TEnumOne = (eOne=1, eTwo, eThree);
type TEnumAlpha = (eAlpha = Ord('A'), eBeta, eGamma);

type 
   TConvert = class
      class function Get(i : Integer) : String;
      begin
         Result:=Chr(i);
      end;
      property Prop[i : Integer] : String read Get;
   end;

PrintLn(Ord(eOne));
PrintLn(eTwo);
PrintLn(IntToStr(eThree));

PrintLn(Chr(Ord(eAlpha)));
PrintLn(Chr(eBeta));
PrintLn(TConvert.Prop[eGamma]);

PrintLn(eOne.Value);
PrintLn(TEnumOne.eTwo.Value.ToString);
var three : TEnumOne := eThree;
PrintLn(three.Value);
