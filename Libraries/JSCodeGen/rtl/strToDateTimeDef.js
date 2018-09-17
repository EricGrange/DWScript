function strToDateTimeDef(fmt, str_dt, def, utc) {
    
    var p = FormatDateTime.cache[fmt], dt;
    if (!p) {
        p = FormatDateTime.Parse(fmt);
        FormatDateTime.cache[fmt] = p;
    }
    
	var year=0, month=0, day=0, hours=0, minutes=0, seconds=0, msec=0,
		i=0, j, k=0, c, tok, value, prevWasHour=0, wasHour=0, d=0;
        
    function getN(n) { k += n; return str_dt.substr(k-n, n);  }
    function getInt(n) { return parseInt(getN(n)); }
    function getVar(n) { 
        var r = ""; 
        while (n > 0 && str_dt.charAt(k) >= "0" && str_dt.charAt(k) <= "9") {
            r += str_dt.charAt(k); 
            k++; n--;
        }
        return parseInt(r);
    }
    function indexOfCI(a) { 
        for (var i = 0; i < a.length; i++) {
            if (str_dt.substr(k, a[i].length).toLowerCase() == a[i].toLowerCase()) {
                k += a[i].length;
                return i;
            }
        }
        return -1;
    }
    
    for (i=0; i < p.length && k < str_dt.length; i++) {
        if (p[i].s) {
            if (str_dt.substr(k, p[i].s.length) != p[i].s) return def;
            k +=  p[i].s.length;
        } else {
            switch (p[i].t) {
                case "yy": 
                    year = getInt(2);
                    var oldK = k, decades = getVar(2);
                    if (!isNaN(decades)) {
                        year = year * (( k == oldK + 1) ? 10 : 100) + decades;
                    } else {
                        var currentYear = (new Date()).getFullYear();
                        year = Math.round(currentYear/100)*100 + year;
                        if (year > currentYear + 50) year -= 100;
                    }
                    if (isNaN(year)) return def;
                    break;
                case "yyyy": 
                    year = getInt(4);
                    if (isNaN(year)) return def;
                    break;
                case "m": 
                    month = getVar(2);
                    if (isNaN(month)) return def;
                    break;
                case "mm": 
                    month = getInt(2);
                    if (isNaN(month)) return def;
                    break;
                case "mmm": 
                    month = indexOfCI($fmt.ShortMonthNames);
                    if (month < 0) return def;
                    break;
                case "mmmm": 
                    month = indexOfCI($fmt.LongMonthNames);
                    if (month < 0) return def;
                    break;
                case "d": 
                    day = getVar(2);
                    if (isNaN(day)) return def;
                    break;
                case "dd": 
                    day = getInt(2);
                    if (isNaN(day)) return def;
                    break;
                case "ddd": 
                    if (indexOfCI($fmt.ShortDayNames) < 0) return def;
                    break;
                case "dddd": 
                    if (indexOfCI($fmt.LongDayNames) < 0) return def;
                    break;
                case "h": 
                    hours = getVar(2);
                    if (isNaN(hours)) return def;
                    break;
                case "hh":
                    hours = getInt(2);
                    if (isNaN(hours)) return def;
                    break;
                case "n": 
                    minutes = getVar(2);
                    if (isNaN(minutes)) return def;
                    break;
                case "nn":
                    minutes = getInt(2);
                    if (isNaN(minutes)) return def;
                    break;
                case "s": 
                    seconds = getVar(2);
                    if (isNaN(seconds)) return def;
                    break;
                case "ss":
                    seconds = getInt(2);
                    if (isNaN(seconds)) return def;
                    break;
                case "z":
                    msec =  getVar(3);
                    if (isNaN(msec)) return def;
                    break;
                case "zzz":
                    msec =  getInt(3);
                    if (isNaN(msec)) return def;
                    break;
                // TODO am/pm, ampm, a/p
            } 
        }
    }
	if ((day | month | year) !== 0) {
		var dt;
		if ((utc||$TZ)==2) {
			dt=Date.UTC(year, month-1, day, hours, minutes, seconds, msec)
		} else {
			dt=new Date(year, month-1, day, hours, minutes, seconds, msec).getTime()
		}
        if (isNaN(dt)) return def;
        //if ((utc||$TZ) === 1) dt -= (new Date).getTimezoneOffset()*60000;
        return dt/864e5+25569;
	} else {
		if ( hours>=0 && hours<24 && minutes>=0 && minutes<60 && seconds>=0 && seconds<60 && msec>=0 && msec<1000 ) {
			return (hours+(minutes+(seconds+msec*0.001)/60)/60)/24
		} else {
			return def;
		}
	}
}
