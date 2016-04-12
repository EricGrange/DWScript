
if not BigInteger(3).IsPrime then PrintLn('bug 3');
if BigInteger(4).IsPrime then PrintLn('bug 4');

//  from Riecoin block 500
var base := BigInteger('40378229068348060265902071710277325464903647442059563624162746921011818446300065249638243017389009856122930741656904767');
if not base.IsPrime then PrintLn('bug base');
if (base+1).IsPrime then PrintLn('bug base+1');
if (base+2).IsPrime then PrintLn('bug base+2');
if (base+3).IsPrime then PrintLn('bug base+3');
if not (base+4).IsPrime then PrintLn('bug base+4');
if (base+5).IsPrime then PrintLn('bug base+5');
if not (base+6).IsPrime then PrintLn('bug base+6');
if (base+8).IsPrime then PrintLn('bug base+8');
if not (base+10).IsPrime then PrintLn('bug base+8');
if not (base+12).IsPrime then PrintLn('bug base+8');
if (base+14).IsPrime then PrintLn('bug base+8');
if not (base+16).IsPrime then PrintLn('bug base+8');
if (base+18).IsPrime then PrintLn('bug base+8');
