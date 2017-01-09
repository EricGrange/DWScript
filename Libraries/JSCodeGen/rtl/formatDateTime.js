function FormatDateTime(fmt, v, u) {
   function pad2(v) { return (v<10)?"0"+v:v; };
   function pad3(v) { return (v<100)?"0"+pad2(v):v; };
   var res="", i=0, c, tok, wasHour, prevWasHour=0, p, dt;
   v = Math.round((v-25569)*864e5);
   dt = new Date(v);
   if ((u||$TZ) === 1) {
     dt = new Date(v - dt.getTimezoneOffset()*60000);
   } 
   if (!(dt instanceof Date && isFinite(dt))) return "Invalid Date";
   while (i<fmt.length) {
      c=fmt.charAt(i);
      tok="";
      do {
         tok+=c;
         i++;
      } while (fmt.charAt(i)===c);
	  wasHour=0;
      switch (tok.toLowerCase()) {
         case "d": res+=(dt.getUTCDate()); break;
         case "dd": res+=pad2(dt.getUTCDate()); break;
         case "ddd": res+=$fmt.ShortDayNames[dt.getUTCDay()]; break;
         case "dddd": res+=$fmt.LongDayNames[dt.getUTCDay()]; break;
         case "m": 
			if (prevWasHour) {
				res+=dt.getUTCHours();
			} else {
				res+=dt.getUTCMonth()+1; 
			};
			break;
         case "mm": 
			if (prevWasHour) {
				res+=pad2(dt.getUTCHours());
			} else {
				res+=pad2(dt.getUTCMonth()+1); 
			};
			break;
         case "mmm": res+=$fmt.ShortMonthNames[dt.getUTCMonth()]; break;
         case "mmmm": res+=$fmt.LongMonthNames[dt.getUTCMonth()]; break;
         case "yy": res+=pad2(dt.getUTCFullYear()%100); break;
         case "yyyy": res+=dt.getUTCFullYear(); break;
         case "h": res+=dt.getUTCHours(); wasHour=1; break;
         case "hh": res+=pad2(dt.getUTCHours()); wasHour=1; break;
         case "n": res+=dt.getUTCMinutes(); break;
         case "nn": res+=pad2(dt.getUTCMinutes()); break;
         case "s": res+=dt.getUTCSeconds(); break;
         case "ss": res+=pad2(dt.getUTCSeconds()); break;
         case "z": res+=dt.getUTCMilliseconds(); break;
         case "zzz": res+=pad3(dt.getUTCMilliseconds()); break;
         default: res+=tok;
      }
	  prevWasHour=wasHour;
   }
   return res;
}
