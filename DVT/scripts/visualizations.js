/*
* here we define the interchangable elements. 
*/

var primaryVisualizationUpto = 4; // based on the #plot

var plotsParameters = new Array(
	"heatMap", new Array("subset", "subsetValue", "answer", "onlyEU"),
	"inCountry", new Array("subset", "country"),
	"crossCountry", new Array("subset", "subsetValue", "country", "countryB"),
	"euBars", new Array("subset", "subsetValue", "onlyEU"),

	"maskMap", new Array(),
	"euMatrix", new Array("onlyEU"),
	"wordMap", new Array("subset", "subsetValue", "answer"),
	"euCompass", new Array("subset", "subsetValue", "answer"),

	"subsetTimeSeries", new Array("subset", "answer", "country")
);

var plotFilesParameters = new Array(
	"heatMap", new Array("dataSource", "subset", "subsetValue", "question", "answer", "locale", "onlyEU"),
	"wordMap", new Array("dataSource", "subset", "subsetValue", "question", "answer", "locale"),
	"euCompass", new Array("dataSource", "subset", "subsetValue", "question", "answer", "locale"),
	"euBars", new Array("dataSource", "subset", "subsetValue", "question", "locale", "onlyEU"),
	"inCountry", new Array("dataSource", "subset", "question", "country", "locale"),
	"crossCountry", new Array("dataSource", "subset", "subsetValue", "question", "country", "countryB", "locale"),
	"euMatrix", new Array("dataSource", "question", "locale", "onlyEU"),

	"subsetTimeSeries", new Array("dataSource", "subset", "question", "answer", "country", "locale")
);

var allParameters = new Array(
	"subset", "subsetValue", "answer", "country", "countryB", "onlyEU"
);

var commonParameters = new Array(
	"dataSource", "question"
);

