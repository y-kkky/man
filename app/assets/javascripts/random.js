function rc(id) {
    var myDiv = document.getElementById(id);
    myDiv.style.borderColor =  "#" + Math.round(
        16777216 * Math.random()
    ).toString(16);
}
