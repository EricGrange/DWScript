CleanupGlobalQueues;

var v : Variant := 123;

Print(GlobalQueuePeek('test', v)); PrintLn(v);
Print(GlobalQueueFirst('test', v)); PrintLn(v);

GlobalQueuePush('test', 456);

Print(GlobalQueuePeek('test', v)); PrintLn(v);
Print(GlobalQueueFirst('test', v)); PrintLn(v);

GlobalQueuePush('test', 'hello');

Print(GlobalQueuePeek('test', v)); PrintLn(v);
Print(GlobalQueueFirst('test', v)); PrintLn(v);

GlobalQueueInsert('test', 'world');

Print(GlobalQueuePeek('test', v)); PrintLn(v);
Print(GlobalQueueFirst('test', v)); PrintLn(v);

GlobalQueuePop('test', v);

Print(GlobalQueuePeek('test', v)); PrintLn(v);
Print(GlobalQueueFirst('test', v)); PrintLn(v);

GlobalQueuePull('test', v);

Print(GlobalQueuePeek('test', v)); PrintLn(v);
Print(GlobalQueueFirst('test', v)); PrintLn(v);

GlobalQueuePop('test', v);

Print(GlobalQueuePeek('test', v)); PrintLn(v);
Print(GlobalQueueFirst('test', v)); PrintLn(v);
