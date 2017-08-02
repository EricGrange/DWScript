CleanupGlobalQueues;

PrintLn(GlobalQueueLength('test'));

GlobalQueuePush('test', 1);
GlobalQueuePush('test', 2);
GlobalQueueInsert('test', 3);

PrintLn(GlobalQueueLength('test'));

GlobalQueueInsert('test', 4);

var v : Variant;

PrintLn(GlobalQueuePull('test', v));
PrintLn(v);

while GlobalQueuePop('test', v) do 
    PrintLn(v);
    
PrintLn(GlobalQueueLength('test'));

PrintLn(GlobalQueuePull('test', v));