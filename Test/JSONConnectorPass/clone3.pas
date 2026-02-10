// === Test 1: Clone object with array of objects, then reassign sub-elements ===
// This triggers the Detach-on-wrong-parent path:
// clonedSubObj.Owner = originalArray (bug), so Detach calls
// originalArray.DetachChild which corrupts the original
var original := JSON.Parse('{"items":[{"id":1,"name":"A"},{"id":2,"name":"B"}]}');
var cloned := original.Clone();

// Access a cloned sub-object and assign it to a new parent
// With the bug: clonedItems[0].Owner = original's items array
// FastWrite calls Detach on the ORIGINAL array, corrupting it
var target := JSON.NewObject();
try
   target.first := cloned.items[0];
   PrintLn('Test1 assign: ' + JSON.Stringify(target));
except
   on E: Exception do
      PrintLn('Test1 assign failed: ' + E.Message);
end;

// Verify original is NOT corrupted (should still have 2 items)
PrintLn('Test1 original intact: ' + IntToStr(original.items.length));

// === Test 2: Clone with nested arrays (array of arrays) ===
// Inner arrays are TdwsJSONArray, so SetOwner circular check is invoked
var nested := JSON.Parse('{"matrix":[[10,20,30],[40,50,60]]}');
var clonedNested := nested.Clone();
var dest := JSON.NewObject();
try
   dest.row := clonedNested.matrix[0];
   PrintLn('Test2 nested array: ' + JSON.Stringify(dest));
except
   on E: Exception do
      PrintLn('Test2 nested array failed: ' + E.Message);
end;
PrintLn('Test2 original intact: ' + IntToStr(nested.matrix.length));

// === Test 3: Clone then drop original, access clone ===
// With the bug, cloned elements have FOwner -> original array.
// When original goes out of scope/gets reassigned, that pointer is stale.
// Accessing clone elements then follows a dangling pointer -> AV
var src := JSON.Parse('{"data":[{"x":1},{"x":2},{"x":3}]}');
var safeClone := src.Clone();
src := JSON.Parse('{}');  // original structure released

// Force access to cloned sub-elements (follows FOwner chain internally)
try
   var s := JSON.Stringify(safeClone);
   PrintLn('Test3 clone after release: ' + s);
except
   on E: Exception do
      PrintLn('Test3 clone after release failed: ' + E.Message);
end;

// Now try to reparent a clone element after original is gone
try
   var dest3 := JSON.NewObject();
   dest3.item := safeClone.data[0];
   PrintLn('Test3 reparent: ' + JSON.Stringify(dest3));
except
   on E: Exception do
      PrintLn('Test3 reparent failed: ' + E.Message);
end;

// === Test 4: Multiple clones, cross-assignment ===
// Clone twice, then move elements between clones
// Each clone's elements should be independent
var base := JSON.Parse('{"arr":[{"v":"alpha"},{"v":"beta"}]}');
var c1 := base.Clone();
var c2 := base.Clone();
base := JSON.Parse('{}');  // release original

try
   var merged := JSON.NewObject();
   merged.a := c1.arr[0];
   merged.b := c2.arr[1];
   PrintLn('Test4 cross-assign: ' + JSON.Stringify(merged));
except
   on E: Exception do
      PrintLn('Test4 cross-assign failed: ' + E.Message);
end;

// === Test 5: Deep nesting - object containing array containing object containing array ===
var deep := JSON.Parse('{"level1":[{"level2":[100,200]},{"level2":[300,400]}]}');
var deepClone := deep.Clone();
deep := JSON.Parse('{}');

try
   PrintLn('Test5 deep access: ' + JSON.Stringify(deepClone.level1[0].level2));
   PrintLn('Test5 deep value: ' + IntToStr(Integer(deepClone.level1[1].level2[1])));
except
   on E: Exception do
      PrintLn('Test5 deep failed: ' + E.Message);
end;

// Reparent deep clone element
try
   var deepDest := JSON.NewObject();
   deepDest.extracted := deepClone.level1[0];
   PrintLn('Test5 deep reparent: ' + JSON.Stringify(deepDest));
except
   on E: Exception do
      PrintLn('Test5 deep reparent failed: ' + E.Message);
end;

// === Test 6: Verify clone independence via modification ===
var modSrc := JSON.Parse('{"tags":["red","green","blue"]}');
var modClone := modSrc.Clone();

modClone.tags[0] := 'yellow';
PrintLn('Test6 original[0]: ' + modSrc.tags[0]);
PrintLn('Test6 clone[0]: ' + modClone.tags[0]);

modSrc.tags[1] := 'purple';
PrintLn('Test6 original[1]: ' + modSrc.tags[1]);
PrintLn('Test6 clone[1]: ' + modClone.tags[1]);

// === Test 7: Clone array at top level (not nested in object) ===
var topArr := JSON.Parse('[{"k":"v1"},{"k":"v2"},{"k":"v3"}]');
var topClone := topArr.Clone();
topArr := JSON.Parse('[]');

try
   PrintLn('Test7 top-level array clone: ' + JSON.Stringify(topClone));
   var dest7 := JSON.NewObject();
   dest7.elem := topClone[1];
   PrintLn('Test7 reparent from array clone: ' + JSON.Stringify(dest7));
except
   on E: Exception do
      PrintLn('Test7 failed: ' + E.Message);
end;

// === Test 8: Repeated clone-and-reparent in a loop (simulates the Laita 60s cycle) ===
var template := JSON.Parse('{"matieres":[{"code":"MP1","nom":"Lait"},{"code":"MP2","nom":"Crème"}]}');
var success := True;
var i : Integer;
for i := 1 to 10 do begin
   try
      var snapshot := template.Clone();
      var output := JSON.NewObject();
      output.timestamp := i;
      output.matiere := snapshot.matieres[0];
      output.allMatieres := snapshot.matieres;
      var s := JSON.Stringify(output);
      if Pos('MP1', s) = 0 then begin
         PrintLn('Test8 iteration ' + IntToStr(i) + ' missing data');
         success := False;
      end;
   except
      on E: Exception do begin
         PrintLn('Test8 iteration ' + IntToStr(i) + ' failed: ' + E.Message);
         success := False;
      end;
   end;
end;
if success then
   PrintLn('Test8 loop (10 iterations): OK');

// Verify template not corrupted after all iterations
PrintLn('Test8 template intact: ' + IntToStr(template.matieres.length));

PrintLn('All tests completed');