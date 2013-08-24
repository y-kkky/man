function highlightOdds(date) {    
    return [true, date.getDate() % 2 == 1 ? 'odd' : ''];
}
