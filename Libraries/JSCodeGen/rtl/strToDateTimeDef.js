function strToDateTimeDef(fmt, str_dt, def, utc) {
	var year=0, month=0, day=0, hours=0, minutes=0, seconds=0, msec=0,
		i=0, j, p=0, c, tok, value, prevWasHour=0, wasHour=0, d=0;
	while (i<fmt.length) {
		c=fmt.charCodeAt(i);
		tok="";
		value=0;
		do {
			tok += fmt[i];
			d = str_dt.charCodeAt(p);
			if (d>=0x30 && d<=0x39) {
				value = value*10 + d - 0x30;
			} else {
				value = -1;
			}
			i++;
			p++;
		} while (fmt.charCodeAt(i)===c);
		tok = tok.toLowerCase();
		var digits = {'m':1,'d':1,'h':1,'n':1,'s':1,'z':2,'yy':2}[tok];
		while (digits>0 && p<str_dt.length) {
			d = str_dt.charCodeAt(p);
			if (d<0x30 || d>0x39) break;
			value = value*10 + d - 0x30;
			p++;
			digits--;
		}

		wasHour=0;
		switch (tok) {
			case "d": case "dd": day=value; break;
			case "ddd":
				j=0;
				while (j<$fmt.hortDayNames.length) {
					if (str_dt.indexOf($fmt.ShortDayNames[j], 0) > 0) {
						p += $fmt.ShortDayNames[j].length - 3;
						break;
					}
					j++;
				}
				break;
			case "dddd":
				j=0;
				while (j<$fmt.LongDayNames.length) {
					if (str_dt.indexOf($fmt.LongDayNames[j],0) > 0){
						p += $fmt.LongDayNames[j].length - 4;
						break;
					}
					j++;
				}
				break;
			case "m": case "mm":
				if (prevWasHour) { 
					minutes=value 
				} else {
					month=value
				}
				break;
			case "mmm":
				j=0;
				while (j<$fmt.ShortMonthNames.length) {
					if (str_dt.indexOf($fmt.ShortMonthNames[j],0) > 0) {
						month=j+1;
						p += $fmt.ShortMonthNames[j].length - 3;
						break;
					}
					j++;
				}
				break;
			case "mmmm":
				j=0;
				while(j<$fmt.LongMonthNames.length){
					if (str_dt.indexOf($fmt.LongMonthNames[j],0) > 0) {
						month=j+1;
						p += $fmt.LongMonthNames[j].length - 4;
						break;
					}
					j++;
				}
				break;
			case "yy": case "yyyy": year=value; break;
			case "h": case "hh": hours=value; wasHour=1; break;
			case "n": case "nn": minutes=value; break;
			case "s": case "ss": seconds=value; break;
			case "z": case "zzz": msec=value; break;
			default:
				var l = tok.length;
				value = str_dt.substring(p-l,p-l+l);
				if (tok!=="?" && tok!=="??" && tok!=="???" && tok!=value) return def;
				wasHour=prevWasHour;
		}
		prevWasHour=wasHour;
	}
	if ((day | month | year) !== 0) {
		var dt;
		if ((utc||$TZ)==2) {
			dt=Date.UTC(year, month-1, day, hours, minutes, seconds, msec)
		} else {
			dt=new Date(year, month-1, day, hours, minutes, seconds, msec).getTime()
		}
		return isNaN(dt) ? def : dt/864e5+25569;
	} else {
		if ( hours>=0 && hours<24 && minutes>=0 && minutes<60 && seconds>=0 && seconds<60 && msec>=0 && msec<1000 ) {
			return (hours+(minutes+(seconds+msec*0.001)/60)/60)/24
		} else {
			return def;
		}
	}
}
