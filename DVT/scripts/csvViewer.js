//kp@eworx.gr EWORX S.A.

var	header = [], data = [], sorted = [], sortColumn = -1, asc = 1;
var	elementJQueryToInsert = "#visualizationData";

function resetCSViewer(){
	$(elementJQueryToInsert).innerHTML = '';
	header = [], data = [], sorted = [], sortColumn = -1, asc = 1;
}




function viewVisualizationData(){
	$.get($("#csvExport").attr("href"), function(data) {
		parseEFCSV(data.replace(/\"/g, '').replace(/\n$/g, ""));
	});

	//$("#visualizationTitle").removeClass("hidden");	
	$("#visualizationData").removeClass("hidden");
	if(!svgSupported)
		$("#visualization").addClass("hidden");
	else
		$("#svgContainer").addClass("hidden");

}

function swapVisualizationData(){
	//$("#visualizationTitle").addClass("hidden");
	if(!svgSupported)
		$("#visualization").removeClass("hidden");
	else
		$("#svgContainer").removeClass("hidden");
		
	$("#visualizationData").addClass("hidden");
}

function parseEFCSV(raw) {
	resetCSViewer();

 	mRows = raw.split("\n");
	for(var mRowIndex=0;mRowIndex<mRows.length;mRowIndex++){
		if(mRowIndex>0){
			data[mRowIndex-1] = [];
		}
		rawMrow = mRows[mRowIndex];
		mRowColumns = rawMrow.split("\t");

		for(var mRowColumnIndex = 0; mRowColumnIndex < mRowColumns.length; mRowColumnIndex++){
			mcell =  mRowColumns[mRowColumnIndex].length == 0 ? '&nbsp;' : mRowColumns[mRowColumnIndex];

			if(mRowIndex == 0){				
				mcell = mcell.replace("CountryCode", "Country").replace("question_code", "Question Id").replace("subset", "Subset").replace("answer", "Answer").replace("percentage", "Percentage");
				header.push('<a href="javascript:" onclick = sortBy(' + mRowColumnIndex + ')>' + mcell + '</a>');
			}else{
				data[mRowIndex-1].push(mcell);
			}
		}
	}
	sorted = data;
	generateHtmlTable();
}

function generateHtmlTable() {


 
	//maintain hidden columns after sorting

	var selectedColumnsId = [];

	$(".table-menu input").each(
	    function(){
	      var $this = $(this);
	      if(!$this.prop("checked"))
	      selectedColumnsId.push($this.attr("id"));
	    }
	);

	//


	var result, $elementJQueryToInsert = $(elementJQueryToInsert)[0];

	$elementJQueryToInsert.innerHTML = '';
	result = '<div class="table-wrapper"><table id="csvData"><thead><tr align="left"><th class="optional">' + header.join('</th><th>') + '</th></tr></thead><tbody>';
	for(mRowIndex = 0; mRowIndex < sorted.length; mRowIndex++) {
		result += '<tr align="left"><td>' + sorted[mRowIndex].join('</td><td>') + '</td></tr>';
	}
	result += '</table></div>';
	$elementJQueryToInsert.innerHTML = result;
	rwdTable();

	for(var i =0;i<selectedColumnsId.length;i++){
		$("#"+selectedColumnsId[i]).click();
	}


}


function indexTable(data){
	var result = [];
	for(var rowIndex=0;rowIndex<data.length;rowIndex++){		
		result[rowIndex] = data[rowIndex];
		result[rowIndex][result[rowIndex].length] = rowIndex;
	}
	return result;
}

function filterArrayTableBy(data, columnIndex, valueToFind){
	var result = [];
	for(var rowIndex=0;rowIndex<data.length;rowIndex++){
		if(data[rowIndex][columnIndex] == valueToFind)
			result[result.length] = data[rowIndex];
	}
	return result;
}

function uniqueArrayTableBy(data, columnIndex){
	var result = [];
	for(var rowIndex=0;rowIndex<data.length;rowIndex++){
		if( jQuery.inArray( data[rowIndex][columnIndex], result )==-1){

			result[result.length] = data[rowIndex][columnIndex];
		}
	}
	return result;
}

function orderArrayTableBy(data, sortColumn, asc) {	 
	return data.sort(function(a, b) {

		var aInfo = parseInfoValue(a[sortColumn])
		var bInfo = parseInfoValue(b[sortColumn]);

		if (aInfo > bInfo) {
			return (asc) ? 1 : -1;
		  }
		if (aInfo < bInfo) {
		  return (asc) ? -1 : 1;
		}
		return 0;
	}); 
}


function sortBy(columnIndex) {
	asc = sortColumn == columnIndex ? !asc:1;
	sortColumn = columnIndex;
	sorted = orderArrayTableBy(data, sortColumn, asc );
	generateHtmlTable();
}

function parseInfoValue(infoValue){
	result = '';
	infoValue = infoValue.replace(',','.');
	if(infoValue != null){
		if(isNaN(infoValue)){
			return infoValue.toUpperCase();
		}
		return parseFloat(infoValue);
	}
	return result;
}
