
PrintLn(GlobalQueueSnapshot('test').Map(lambda (v) => String(v)).join(','));

GlobalQueuePush('test', 123);

PrintLn(GlobalQueueSnapshot('test').Map(lambda (v) => String(v)).join(','));

GlobalQueuePush('test', 456);
GlobalQueuePush('test', 'hello');

PrintLn(GlobalQueueSnapshot('test').Map(lambda (v) => String(v)).join(','));

var v : Variant;
GlobalQueuePull('test', v);

PrintLn(GlobalQueueSnapshot('test').Map(lambda (v) => String(v)).join(','));