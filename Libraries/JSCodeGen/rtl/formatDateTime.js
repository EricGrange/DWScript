function FormatDateTime(fmt, v, u) {
   // parse format
   var p = FormatDateTime.cache[fmt], dt, convertToLocal;
   if (!p) {
      p = FormatDateTime.Parse(fmt);
      FormatDateTime.cache[fmt] = p;
   }
   // parse date
   if (v instanceof Date) {
      dt = v;
      convertToLocal = !(u==1);
   } else {
      dt = new Date(Math.round((v-25569)*864e5));
      convertToLocal = !(((u||$TZ)==1) || (Math.abs(v) < 1));
   }
   // apply format
   function pad2(v) { return v < 10 ? "0" + v  : v; }
   function pad3(v) { return v < 10 ? "00" + v  : v < 100 ? "0" + v : v; }
   var r = "";
   for (var i = 0; i < p.length; i++) {
      if (p[i].s) {
         r += p[i].s;
      } else {
         switch (p[i].t) {
            case "d": r += convertToLocal ? dt.getDate() : dt.getUTCDate(); break;
            case "dd": r += pad2(convertToLocal ? dt.getDate() : dt.getUTCDate()); break;
            case "ddd": r += $fmt.ShortDayNames[convertToLocal ? dt.getDay() : dt.getUTCDay()]; break;
            case "dddd": r += $fmt.LongDayNames[convertToLocal ? dt.getDay() : dt.getUTCDay()]; break;
            case "m": r += convertToLocal ? dt.getMonth()+1 : dt.getUTCMonth()+1; break;
            case "mm": r += pad2(convertToLocal ? dt.getMonth()+1 : dt.getUTCMonth()+1); break;
            case "mmm": r += $fmt.ShortMonthNames[convertToLocal ? dt.getMonth() : dt.getUTCMonth()]; break;
            case "mmmm": r += $fmt.LongMonthNames[convertToLocal ? dt.getMonth() : dt.getUTCMonth()]; break;
            case "yy": r += pad2(convertToLocal ? dt.getFullYear()%100 : dt.getUTCFullYear()%100); break;
            case "yyyy": r += convertToLocal ? dt.getFullYear() : dt.getUTCFullYear(); break;
            case "h": r += convertToLocal ? dt.getHours() : dt.getUTCHours(); break;
            case "hh": r += pad2(convertToLocal ? dt.getHours() : dt.getUTCHours()); break;
            case "_h": r += (((convertToLocal ? dt.getHours() : dt.getUTCHours()) + 11) % 12)+1; break;
            case "_hh": r += pad2((((convertToLocal ? dt.getHours() : dt.getUTCHours()) + 11) % 12)+1); break;
            case "n": r += convertToLocal ? dt.getMinutes() : dt.getUTCMinutes(); break;
            case "nn": r += pad2(convertToLocal ? dt.getMinutes() : dt.getUTCMinutes()); break;
            case "s": r += convertToLocal ? dt.getSeconds() : dt.getUTCSeconds(); break;
            case "ss": r += pad2(convertToLocal ? dt.getSeconds() : dt.getUTCSeconds()); break;
            case "z": r += convertToLocal ? dt.getMilliseconds() : dt.getUTCMilliseconds(); break;
            case "zzz": r += pad3(convertToLocal ? dt.getMilliseconds() : dt.getUTCMilliseconds()); break;
            case "ampm": 
               r += (((convertToLocal ? dt.getHours() : dt.getUTCHours())||24) <= 12) ? $fmt.TimeAMString : $fmt.TimePMString;
               break; 
            case "am/pm": 
               r += (((convertToLocal ? dt.getHours() : dt.getUTCHours())||24) <= 12) ? "am" : "pm";
               break;
            case "a/p": 
               r += (((convertToLocal ? dt.getHours() : dt.getUTCHours())||24) <= 12) ? "a" : "p";
               break;
         }
      }
   }
   return r;
}
FormatDateTime.Parse = function (fmt) {
   var tok = ['d','dd','ddd','dddd','m','mm','mmm','mmmm','yy','yyyy',
              'h','hh','n','nn','s','ss','z','zzz',
              'ampm','am/pm','a/p'];
   var r = [], lit = '', wasHour = 0;
   for (var i = 0; i < fmt.length;) {
      if (fmt.charAt(i) == '"' || fmt.charAt(i) == "'") {
         var quote = fmt.charAt(i++), p = i;
         while (i < fmt.length && fmt.charAt(i) != quote) i++;
         lit += fmt.substring(p, i++);
         continue;
      }
      var bt = ''
      for (var k = 0; k < tok.length; k++) {
         if (tok[k].length <= bt.length) continue;
         if (fmt.substr(i, tok[k].length).toLowerCase() == tok[k]) bt = tok[k];
      }
      if (bt == '') {
         lit += fmt.charAt(i);
         i++;
      } else {
         if (lit != '') {
            r.push({s:lit});
            lit = '';
         }
         if (bt.charAt(0) == 'a') {
            for (var j = r.length-1; j >= 0; j--) {
               if (('t' in r[j]) && (r[j].t == 'h' || r[j].t == 'hh')) {
                  r[j].t = '_' + r[j].t;
                  break;
               }
            }
         } else if (wasHour) {
            if (bt == 'mm') bt = 'nn';
            if (bt == 'm') bt = 'n';
         }
         wasHour = (bt == 'hh' || bt == 'h');
         r.push({t:bt});
         i += bt.length;
      }
   }
   if (lit !== '') r.push({s:lit});
   return r;
}
FormatDateTime.cache = {};
