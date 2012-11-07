var v := JSON.Parse(#'
   {
   "hello":"world",
   "123":[1,2,3],
   "sub":
      {
      "field":456,
      "objs":[
         {"obj1":1},
         {"obj2":2}
         ]
      }
   }');

PrintLn(v.hello);

var v123 := v['123'];
PrintLn(v123.typename());
PrintLn(v123.length());

var i : Integer;
for i:=v123.low() to v123.high() do begin
   Print(v123.ElementName(i));
   Print(': ');
   PrintLn(v123[i]);
end;

PrintLn(v.sub.typename());
PrintLn(v.sub.field);
PrintLn(v.sub.objs[0].obj1);
PrintLn(v['sub']['objs'][1]['obj2']);
