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

var v2 := v.clone();

PrintLn(JSON.stringify(v2));
