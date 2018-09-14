function FormatDateTime(fmt, v, u) {
    
    function pad2(v) { return v < 10 ? "0" + v  : v; }
    function pad3(v) { return v < 10 ? "00" + v  : v < 100 ? "0" + v : v; }
    function Parse(fmt) {
        var tok = ['d','dd','ddd','dddd','m','mm','mmm','mmmm','yy','yyyy',
                   'h','hh','n','nn','s','ss','z','zzz',
                   'ampm','am/pm','a/p'];
        var r = [], lit = '';
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
                }
                r.push({t:bt});
                i += bt.length;
            }
        }
        if (lit !== '') r.push({s:lit});
        return r;
    }
    var p = FormatDateTime.cache[fmt], dt;
    if (!p) {
        p = Parse(fmt);
        FormatDateTime.cache[fmt] = p;
    }
    if (v >=0 && v < 1) {
        dt = new Date();
        dt.setUTCHours(0,0,0,0);
        dt = new Date(dt.getTime() + Math.round(v*864e5));
    } else {
        v = Math.round((v-25569)*864e5);
        dt = new Date(v);
        if ((u||$TZ) === 1) {
            dt = new Date(v - dt.getTimezoneOffset()*60000);
        } 
    }
    var r = '', wasHour, prevWasHour = 0;
    for (var i = 0; i < p.length; i++) {
        if (p[i].s) {
            r += p[i].s;
        } else {
            wasHour = 0;
            switch (p[i].t) {
                case "d": r += dt.getUTCDate(); break;
                case "dd": r += pad2(dt.getUTCDate()); break;
                case "ddd": r += $fmt.ShortDayNames[dt.getUTCDay()]; break;
                case "dddd": r += $fmt.LongDayNames[dt.getUTCDay()]; break;
                case "m": 
                    if (prevWasHour) {
                        r += dt.getUTCHours();
                    } else {
                        r += dt.getUTCMonth()+1; 
                    };
                    break;
                case "mm": 
                    if (prevWasHour) {
                        r += pad2(dt.getUTCHours());
                    } else {
                        r += pad2(dt.getUTCMonth()+1); 
                    };
                    break;
                case "mmm": r += $fmt.ShortMonthNames[dt.getUTCMonth()]; break;
                case "mmmm": r += $fmt.LongMonthNames[dt.getUTCMonth()]; break;
                case "yy": r += pad2(dt.getUTCFullYear()%100); break;
                case "yyyy": r += dt.getUTCFullYear(); break;
                case "h": r += dt.getUTCHours(); wasHour=1; break;
                case "hh": r += pad2(dt.getUTCHours()); wasHour=1; break;
                case "_h": r += ((dt.getUTCHours() + 11) % 12)+1; wasHour=1; break;
                case "_hh": r += pad2(((dt.getUTCHours() + 11) % 12)+1); wasHour=1; break;
                case "n": r += dt.getUTCMinutes(); break;
                case "nn": r += pad2(dt.getUTCMinutes()); break;
                case "s": r += dt.getUTCSeconds(); break;
                case "ss": r += pad2(dt.getUTCSeconds()); break;
                case "z": r += dt.getUTCMilliseconds(); break;
                case "zzz": r += pad3(dt.getUTCMilliseconds()); break;
                case "ampm": 
                    if (dt.getUTCHours() >= 1 && dt.getUTCHours() <= 12) {
                        r += $fmt.TimeAMString;
                    } else {
                        r += $fmt.TimePMString;
                    };
                    break; 
                case "am/pm": 
                    r += (dt.getUTCHours() >= 1 && dt.getUTCHours() <= 12) ? 'am' : 'pm';
                    break;
                case "a/p": 
                    r += (dt.getUTCHours() >= 1 && dt.getUTCHours() <= 12) ? 'a' : 'p';
                    break;
            }
            prevWasHour=wasHour;
        }
    }
    return r;
}
FormatDateTime.cache = {};