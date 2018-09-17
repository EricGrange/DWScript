function ByteSizeToStr(s, u) {
   var r = '', a = Math.abs(s); 
   if (a < 1024) {
       r = s + ' '
   } else if (a < 1024*1024) {
       r = (s/1024).toFixed(1) + ' k'
   } else if (a < 1024*1024*1024) {
       r = (s/(1024*1024)).toFixed(2) + ' M'
   } else if (a < 1024*1024*1024*1024) {
       r = (s/(1024.0*1024*1024)).toFixed(2) + ' G'
   } else {
       r = (s/(1024.0*1024*1024*1024)).toFixed(2) + ' T'
   }
   return r + (u || 'B');
}