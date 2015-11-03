/*Scriptsforthetablestestpage
	Author:MaggieWachs,www.filamentgroup.com
	Date:November2011
	Dependencies:jQuery,jQueryUIwidgetfactory
	Author:kp@eworx.gr, som@eworx.gr, cp@eworx.gr EWORX S.A.
*/


function initRWDTable(){
	$.widget("filament.table",{//needtocomeupwithabetternamespacevar...

	options:{
		idprefix:null,	//specifyaprefixfortheid/headersvalues
		persist:null,//specifyaclassassignedtocolumnheaders(th)thatshouldalwaysbepresent;thescriptnotcreateacheckboxforthesecolumns
		checkContainer:null//containerelementwherethehide/showcheckboxeswillbeinserted;ifnonespecified,thescriptcreatesamenu
	},

	//Setupthewidget
	_create:function(){
		varself=this,
			o=$(".table-wrapper"),
			table=$("#csvData");
			thead=table.find("thead"),
			tbody=table.find("tbody"),
			hdrCols=thead.find("th"),
			bodyRows=tbody.find("tr"),
			container=o.checkContainer?$(o.checkContainer):$('<div class="table-menu table-menu-hidden"><ul/></div>');		
		
		//addclassforscopingstyles-cellsshouldbehiddenonlywhenJSison
		table.addClass("enhanced");
		
		hdrCols.each(function(i){
			var th = $(this),
					id = th.attr("id"), 
					classes = th.attr("class");
			
			// assign an id to each header, if none is in the markup
			if (!id) {
				id = ( o.idprefix ? o.idprefix : "col-" ) + i;
				th.attr("id", id);
			};
		
		//assignmatching"headers"attributestotheassociatedcells
		//TEMP-needstobeeditedtoaccommodatecolspans
			bodyRows.each(function(){
				var cell = $(this).find("th, td").eq(i);								
				cell.attr("headers", id);
				if (classes) { cell.addClass(classes); };
			}); 
		
			// create the hide/show toggles
			if ( !th.is("." + o.persist) ) {
				var toggle = $('<li><input type="checkbox" name="toggle-cols" id="toggle-col-'+i+'" value="'+id+'" /> <label for="toggle-col-'+i+'">'+th.text()+'</label></li>');
				
				container.find("ul").append(toggle);			
				
				toggle.find("input")
					.change(function(){
						var input = $(this), 
							val = input.val(), 
							cols = $("#" + val + ", [headers="+ val +"]");
						
						if (input.is(":checked")) { cols.show(); }
						else { cols.hide(); };		
					})
					.bind("updateCheck", function(){
						if ( th.css("display") == "table-cell" || th.css("display") == "inline" ) {
							$(this).attr("checked", true);
						}
						else {
							$(this).attr("checked", false);
						}
					})
					.trigger("updateCheck");  
			};			 
					
		}); // end hdrCols loop 

		//updatetheinputs'checkedstatus
		$(window).bind("orientationchangeresize",function(){
		container.find("input").trigger("updateCheck");
		});

		//ifnocontainerspecifiedforthecheckboxes,createa"Display"menu
		// if no container specified for the checkboxes, create a "Display" menu		
		if (!o.checkContainer) {
			var menuWrapper = $('<div class="table-menu-wrapper" />'),
					menuBtn = $('<a href="#" class="table-menu-btn">Display</a>');
					
			menuBtn.click(function(){
				container.toggleClass("table-menu-hidden");				
				return false;
			});
					
			menuWrapper.append(menuBtn).append(container);
			table.before(menuWrapper);
			
			// assign click-to-close event
			$(document).click(function(e){								
				if ( !$(e.target).is( container ) && !$(e.target).is( container.find("*") ) ) {			
					container.addClass("table-menu-hidden");
				}				
			});
		};	
		
				  
	}, // end _create
	
	disable:function(){
		//TBD
	},

	enable:function(){
		//TBD
	}
	
	});

}

function rwdTable(){
	initRWDTable();
	$("#csvData").table({
		idprefix: "co-",
		persist: "persist"
	});
}
