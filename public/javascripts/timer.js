start = Date.getTime();
writeCookie("start", start, 1);

function otmena(){
    window.onbeforeunload = function(){
	
    }
    if(document.title!="Щоденне змагання"){
	if(confirm("Ви впевнені, що хочете завершити розв'язання тесту?"))
	    return true;
	else
	    return false;
    }
}

function starter(){
    if(document.title!="Щоденне змагання"){
	if(!confirm("Розпочати розв'язання тесту з урахуванням часу?")){
	    var timer = document.getElementById("timer");
	    timer.parentNode.removeChild(timer);
	}else{
	    startTimer();
	}
    }else{
	startTimer();
    }
}
function startTimer() {
    var timer = document.getElementById("timer");
    var time = timer.innerHTML;
    var arr = time.split(":");
    var h = arr[0];
    var m = arr[1];
    var s = arr[2];
    if (s == 0) {
	if (m == 0) {
	    if (h == 0) {
		window.onbeforeunload = function(){
		}
		document.forms["test"].submit();
		return;
	    }
	    h--;
	    m = 60;
	    if (h < 10) h = "0" + h;
	}
	m--;
	if (m < 10) m = "0" + m;
	s = 59;
    }
    else s--;
    if (s < 10) s = "0" + s;
    document.getElementById("timer").innerHTML = h+":"+m+":"+s;
    setTimeout(startTimer, 1000);
}
if(document.title != "Щоденне змагання"){
    window.onbeforeunload = function(){
	return "Ви впевнені, що хочете піти з цієї сторінки? Ваші дані не будуть збережені!";
    }
}

window.onunload = function(){
	end = Date.getTime();
	writeCookie("end", end, 1)
    }

function writeCookie(name,value,days) {
    var date, expires;
    if (days) {
        date = new Date();
        date.setTime(date.getTime()+(days*24*60*60*1000));
        expires = "; expires=" + date.toGMTString();
            }else{
        expires = "";
    }
    document.cookie = name + "=" + value + expires + "; path=/";
}
