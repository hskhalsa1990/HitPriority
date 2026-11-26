

find.columns<-function(colnames=minimal.cols,col="Potency_Molar"){
  grep(col,colnames)
}



sci.format.single.column<-function(colposition, fixed_digits=2){
  js <- c(
    "function(row, data, displayNum, index){",
    paste0("var x = data[", colposition,"];",collapse=""),
    "if (Math.abs(x)>1000 | Math.abs(x)<0.01){",
    paste0("$('td:eq(", colposition, ")', row).html(x.toExponential(2));",collapse=""),
    "} else {",
    paste0("$('td:eq(", colposition, ")', row).html(x.toFixed(", fixed_digits, "));",collapse=""),
    "}",
    "}"
  )
  return(js)
}



sci.format.multiple.columns<-function(colpositions, fixed_digits=2){
  js <- c(
    "function(row, data) {",
    paste0 ("var selcol =[", paste0(colpositions,collapse=","),"];",collapse=""),
    "var arrayLength = selcol.length;",
    "for (i = 0; i < arrayLength; i++) {",
    "var j = selcol[i];",
    "if (data[j] !== null & data[j] !== '' & data[j]!=='NA') {",
    "if (Math.abs(data[j])>1000 | Math.abs(data[j])<0.01) {",
    "$('td:eq('+j+')', row).html(data[j].toExponential(2));",
    "} else  if ((Math.abs(data[j])<1000 & Math.abs(data[j])>0.01) ){",
    paste0("$('td:eq('+j+')', row).html(data[j].toFixed(", fixed_digits, "));",collapse=""),
    "}",
    "}",
    "}",
    "}")
  return(js)
}
