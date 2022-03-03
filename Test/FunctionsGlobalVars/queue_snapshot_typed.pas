CleanupGlobalQueues();

GlobalQueuePush('int', 123);
GlobalQueuePush('int', 456);

GlobalQueuePush('str', 'hello');
GlobalQueuePush('str', 'world');

GlobalQueuePush('float', 1.75);
GlobalQueuePush('float', 2.25);

var aInt := GlobalQueueSnapshotIntegers('int');
PrintLn(JSON.Stringify(aInt));

var aStr := GlobalQueueSnapshotStrings('str');
PrintLn(JSON.Stringify(aStr));

var aFlo := GlobalQueueSnapshotFloats('float');
PrintLn(JSON.Stringify(aFlo));

PrintLn('floats from ints');
aFlo := GlobalQueueSnapshotFloats('int');
PrintLn(JSON.Stringify(aFlo));

PrintLn('strings from floats');
aStr := GlobalQueueSnapshotStrings('float');
PrintLn(JSON.Stringify(aStr));

PrintLn('ints from floats');
aInt := GlobalQueueSnapshotIntegers('float');
PrintLn(JSON.Stringify(aInt));

PrintLn('ints from strings');
try
   aInt := GlobalQueueSnapshotIntegers('str');
except
   on E: Exception do
      PrintLn(E.Message)
end;

PrintLn(JSON.Stringify(aInt));