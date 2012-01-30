{if Min(1, 2)<>1 then PrintLn('failed Min 1, 2');
if Min(2, 1)<>1 then PrintLn('failed Min 2, 1');
if Max(1, 2)<>2 then PrintLn('failed Max 1, 2');
if Max(2, 1)<>2 then PrintLn('failed Max 2, 1');
}
if Min(1.5, 2.5)<>1.5 then PrintLn('failed Min 1.5, 2.5');
if Min(2.5, 1.5)<>1.5 then PrintLn('failed Min 2.5, 1.5');
if Max(1.5, 2.5)<>2.5 then PrintLn('failed Max 1.5, 2.5');
if Max(2.5, 1.5)<>2.5 then PrintLn('failed Max 2.5, 1.5');


if MinInt(1, 2)<>1 then PrintLn('failed MinInt 1, 2');
if MinInt(2, 1)<>1 then PrintLn('failed MinInt 2, 1');
if MaxInt(1, 2)<>2 then PrintLn('failed MaxInt 1, 2');
if MaxInt(2, 1)<>2 then PrintLn('failed MaxInt 2, 1');
