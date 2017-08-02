
for var i := 1 to 100 do begin
    GlobalQueuePush('p', i);
    GlobalQueueInsert('p', -i);
end;

while GlobalQueueLength('p') > 0 do begin
    var v : Variant;
    if GlobalQueuePop('p', v) then
        Print(v);
    if GlobalQueuePull('p', v) then
        PrintLn(v);
end;