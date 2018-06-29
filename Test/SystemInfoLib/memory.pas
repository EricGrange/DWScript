
Print('Working set size in 1..100 MB range: ');
var wssMegabytes := ApplicationInfo.MemoryCounters.WorkingSetSize shr 20;
PrintLn(if wssMegabytes in [1..100] then 'passed' else 'failed');

var ms := new MemoryStatus;

if (ms.Virtual.Total shr 30) not in [1..4] then begin
    // 64 bit address space
    if ms.Physical.Total > ms.Virtual.Total then
      PrintLn('Expected physical size to be less than virtual size in 64bit app');
end;

PrintLn(ms.Virtual.Available < ms.Virtual.Total);
PrintLn(ms.Physical.Available < ms.Physical.Total);


