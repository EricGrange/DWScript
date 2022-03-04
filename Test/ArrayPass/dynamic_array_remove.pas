
var arr := ('a,b').Split(',');
var removedA := arr.Remove('a') >= 0;

PrintLn(removedA);

PrintLn(arr.join(','));

removedA := arr.Remove('a') >= 0;

PrintLn(removedA);
