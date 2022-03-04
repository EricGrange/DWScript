var list: array of integer = [5, 7];
var item: variant = 5;
if item in list then
   println('success')
else println('fail');

if not Integer(item) in list then
   println('bug');
   
if not 5 in list then
   println('bug');
   
if not (not 5) in list then
   println('ok');