var neighborhoodAreas = new Object();
var rentalMarkers = new Object();
overlay_db = {};

/*-----------------------------------------------------------------*/
/* Icons */
var apartmentIcon = new GIcon('http://steve.livesqft.com/public/img/map_icons/apartment.png');
apartmentIcon.shadow = 'http://steve.livesqft.com/public/img/dot.gif';
apartmentIcon.iconSize = new GSize(32, 37);
apartmentIcon.shadowSize = new GSize(1,1);
apartmentIcon.iconAnchor = new GPoint(17, 36);
apartmentIcon.infoWindowAnchor = new GPoint(21, 11);
apartmentIcon.printImage = 'http://steve.livesqft.com/public/img/map_icons/apartment.png';
apartmentIcon.mozPrintImage = 'http://steve.livesqft.com/public/img/map_icons/apartment.png';
apartmentIcon.printShadow = 'http://steve.livesqft.com/public/img/dot.gif';
apartmentIcon.transparent = 'http://steve.livesqft.com/public/img/map_icons/apartment.png';
apartmentIcon.imageMap = Number[3,3, 28,3, 28,28, 3,28];
apartmentIcon.maxHeight = 6;

var crimeIcon = new GIcon('http://steve.livesqft.com/public/img/map_icons/police.png');
crimeIcon.shadow = 'http://steve.livesqft.com/public/img/dot.gif';
crimeIcon.iconSize = new GSize(32, 37);
crimeIcon.shadowSize = new GSize(1,1);
crimeIcon.iconAnchor = new GPoint(17, 36);
crimeIcon.infoWindowAnchor = new GPoint(21, 11);
crimeIcon.printImage = 'http://steve.livesqft.com/public/img/map_icons/police.png';
crimeIcon.mozPrintImage = 'http://steve.livesqft.com/public/img/map_icons/police.png';
crimeIcon.printShadow = 'http://steve.livesqft.com/public/img/dot.gif';
crimeIcon.transparent = 'http://steve.livesqft.com/public/img/map_icons/police.png';
crimeIcon.imageMap = Number[3,3, 28,3, 28,28, 3,28];
crimeIcon.maxHeight = 6;

var diningIcon = new GIcon('http://steve.livesqft.com/public/img/map_icons/restaurant.png');
diningIcon.shadow = 'http://steve.livesqft.com/public/img/dot.gif';
diningIcon.iconSize = new GSize(32, 37);
diningIcon.shadowSize = new GSize(1,1);
diningIcon.iconAnchor = new GPoint(17, 36);
diningIcon.infoWindowAnchor = new GPoint(21, 11);
diningIcon.printImage = 'http://steve.livesqft.com/public/img/map_icons/restaurant.png';
diningIcon.mozPrintImage = 'http://steve.livesqft.com/public/img/map_icons/restaurant.png';
diningIcon.printShadow = 'http://steve.livesqft.com/public/img/dot.gif';
diningIcon.transparent = 'http://steve.livesqft.com/public/img/map_icons/restaurant.png';
diningIcon.imageMap = Number[3,3, 28,3, 28,28, 3,28];
diningIcon.maxHeight = 6;

var schoolIcon = new GIcon('http://steve.livesqft.com/public/img/map_icons/school.png');
schoolIcon.shadow = 'http://steve.livesqft.com/public/img/dot.gif';
schoolIcon.iconSize = new GSize(32, 37);
schoolIcon.shadowSize = new GSize(1,1);
schoolIcon.iconAnchor = new GPoint(17, 36);
schoolIcon.infoWindowAnchor = new GPoint(21, 11);
schoolIcon.printImage = 'http://steve.livesqft.com/public/img/map_icons/school.png';
schoolIcon.mozPrintImage = 'http://steve.livesqft.com/public/img/map_icons/school.png';
schoolIcon.printShadow = 'http://steve.livesqft.com/public/img/dot.gif';
schoolIcon.transparent = 'http://steve.livesqft.com/public/img/map_icons/school.png';
schoolIcon.imageMap = Number[3,3, 28,3, 28,28, 3,28];
schoolIcon.maxHeight = 6;

var marketIcon = new GIcon('http://steve.livesqft.com/public/img/map_icons/supermarket.png');
marketIcon.shadow = 'http://steve.livesqft.com/public/img/dot.gif';
marketIcon.iconSize = new GSize(32, 37);
marketIcon.shadowSize = new GSize(1,1);
marketIcon.iconAnchor = new GPoint(17, 36);
marketIcon.infoWindowAnchor = new GPoint(21, 11);
marketIcon.printImage = 'http://steve.livesqft.com/public/img/map_icons/supermarket.png';
marketIcon.mozPrintImage = 'http://steve.livesqft.com/public/img/map_icons/supermarket.png';
marketIcon.printShadow = 'http://steve.livesqft.com/public/img/dot.gif';
marketIcon.transparent = 'http://steve.livesqft.com/public/img/map_icons/supermarket.png';
marketIcon.imageMap = Number[3,3, 28,3, 28,28, 3,28];
marketIcon.maxHeight = 6;

var nightlifeIcon = new GIcon('http://steve.livesqft.com/public/img/map_icons/bar.png');
nightlifeIcon.shadow = 'http://steve.livesqft.com/public/img/dot.gif';
nightlifeIcon.iconSize = new GSize(32, 37);
nightlifeIcon.shadowSize = new GSize(1,1);
nightlifeIcon.iconAnchor = new GPoint(17, 36);
nightlifeIcon.infoWindowAnchor = new GPoint(21, 11);
nightlifeIcon.printImage = 'http://steve.livesqft.com/public/img/map_icons/bar.png';
nightlifeIcon.mozPrintImage = 'http://steve.livesqft.com/public/img/map_icons/bar.png';
nightlifeIcon.printShadow = 'http://steve.livesqft.com/public/img/dot.gif';
nightlifeIcon.transparent = 'http://steve.livesqft.com/public/img/map_icons/bar.png';
nightlifeIcon.imageMap = Number[3,3, 28,3, 28,28, 3,28];
nightlifeIcon.maxHeight = 6;

/*-----------------------------------------------------------------*/
/* Controls */
function DataOverlayControl() {
}

DataOverlayControl.prototype = new GControl();

DataOverlayControl.prototype.initialize = function(map) {
	var container = document.createElement("div");
	container.className = 'ui-state-default ui-corner-all ui-map-button';
	$(container).css({'font-size' : '.80em', 'padding' : '4px'});
	var span = document.createElement("span");
	span.className = 'ui-icon ui-icon-info';
	$(span).css({'float': 'left', 'margin-right' : '6px'});
	container.appendChild(span);
	$(container).append('Show...<br clear="both" />');
	var overlaysContainer = document.createElement("div");
	$(overlaysContainer).attr('id', 'overlayToggles');
	$(overlaysContainer).css('display', 'none');
	$(overlaysContainer).append('<input type="checkbox" id="crime" onClick="getCrimePoints();"><label for="crime">Crime</label><br />');
	$(overlaysContainer).append('<input type="checkbox" id="dining" onClick="getDiningPoints();"><label for="dining">Dining</label><br />');
	$(overlaysContainer).append('<input type="checkbox" id="markets" onClick="getMarketPoints();"><label for="markets">Markets</label><br />');
	$(overlaysContainer).append('<input type="checkbox" id="nightlife" onClick="getNightlifePoints();"><label for="nightlife">Nightlife</label><br />');
	$(overlaysContainer).append('<input type="checkbox" id="fitness" onClick="getFitnessPoints();"><label for="fitness">Health & Fitness</label><br />');
	$(overlaysContainer).append('<input type="checkbox" id="schools" onClick="getSchoolPoints();"><label for="schools">Schools</label><br />');
	$(container).append($(overlaysContainer));
	
	$(container).hover(
		function() {
			$("#overlayToggles").slideDown("fast");
		},
		function() {
			$("#overlayToggles").slideUp("fast");
		}
	);
			
	map.getContainer().appendChild(container);
	return container;
}


DataOverlayControl.prototype.getDefaultPosition = function() {
  	return new GControlPosition(G_ANCHOR_TOP_RIGHT, new GSize(7, 7));
}

function SearchControl(){
}

SearchControl.prototype = new GControl();

SearchControl.prototype.initialize = function(map){
	var container = document.createElement("div");
	container.className = 'ui-state-default ui-corner-all ui-map-button';
	$(container).css({'padding' : '4px', 'width': '220px'});
	$(container).append('<div id="currentSearches" style="display:none;"></div>');
	var input = document.createElement("input");
	$(input).attr({'type': 'text', 'id' : 'localSearchInput'});
	$(input).css({'float': 'left', 'padding': '2px'});
	container.appendChild(input);
	$(container).append('<input type="button" value="Search" onClick="localSearch();" />');
	
	map.getContainer().appendChild(container);
	return container;
}

SearchControl.prototype.getDefaultPosition = function() {
  	return new GControlPosition(G_ANCHOR_BOTTOM_RIGHT, new GSize(0, 0));
}


/*-----------------------------------------------------------------*/
/* Init Functions */
function load() {
  if (GBrowserIsCompatible()) {
	gmap = new GMap2(document.getElementById("map"));
	gmap.setCenter(new GLatLng(39.289525, -76.615248), 14);								
	gmap.addControl(new GSmallZoomControl3D());
	gmap.addControl(new GScaleControl());
	gmap.addControl(new DataOverlayControl());
	gmap.addControl(new SearchControl());
	
	initLocalSearch();
	
	if(typeof(rentals) !== 'undefined'){
		addRentalsToMap(rentals);
	}
  }
}

function addLoadEvent(func) { 
	var oldonload = window.onload; 
	if (typeof window.onload != 'function'){ 
		window.onload = func
	} else { 
		window.onload = function() {
			oldonload();
			func();
		}
	}
}

function addUnLoadEvent(func) { 
	var oldonunload = window.onunload; 
	if (typeof window.onunload != 'function'){ 
		window.onunload = func
	} else { 
		window.onunload = function() {
			oldonunload();
			func();
		}
	}
}
 
addLoadEvent(load);
addUnLoadEvent(GUnload);


/*-----------------------------------------------------------------*/
/* POI functions */
function make_bits(id, url, make_marker_function, make_window_function) {
	 if(!isMapOverlay(id)){		
		$.getJSON(url, function(data) {
			$.each(data, function(idx) {
				var mark = make_marker_function(this);				
				make_window_function(mark);
				
				storeMarker(id, mark);				
				gmap.addOverlay(mark);
			});
		});
	 }
}

function isMapOverlay(id){
	if (!overlay_db[id]){
		overlay_db[id] = [];
		return false;
	}
	//if it's already loaded, unload:
	else if (overlay_db[id].length > 0) {
		$.each(overlay_db[id], function() { gmap.removeOverlay(this); });
		delete overlay_db[id];
		return true;
	}	
}

function storeMarker(id, marker){
	overlay_db[id].push(marker);	
}

function getCrimePoints(){
    make_bits("#crimes", "/debug/crimes", function(foo) {
        return new GMarker(new GLatLng(foo[0], foo[1]), {'title': foo[2], 'icon': crimeIcon });
    }, 
	function(mark){
		GEvent.addListener(mark, 'click', function(){ 
			mark.openExtInfoWindow(
				gmap,
				"crime_window",
				"<div>A nefarious crime</div>",
				{beakOffset: 0}
			); 
		});	
	});
}

function getDiningPoints(){
    make_bits("dining", "/search/points_of_interest?type=dining&start=0&count=150", function(foo) {
        return new GMarker(new GLatLng(foo.latitude, foo.longitude), {'title': foo.name, 'icon': diningIcon });
    }, 
	function(mark){
		GEvent.addListener(mark, 'click', function(){ 
			mark.openExtInfoWindow(
				gmap,
				"dining_window",
				"<div>This is a dining spot</div>",
				{beakOffset: 0}
			); 
		});	
	});
}

function getSchoolPoints(){
    make_bits("#schools", "/debug/schools", function(foo) {
        return new GMarker(new GLatLng(foo[1], foo[2]), {'title': foo[0], 'icon': schoolIcon });
    }, 
	function(mark){
		GEvent.addListener(mark, 'click', function(){ 
			mark.openExtInfoWindow(
				gmap,
				"school_window",
				"<div>This is a school</div>",
				{beakOffset: 0}
			); 
		});	
	});
}

function getMarketPoints(){
    make_bits("#markets", "/debug/markets", function(foo) {
        return new GMarker(new GLatLng(foo[1], foo[2]), {'title': foo[0], 'icon': marketIcon });
    }, 
	function(mark){
		GEvent.addListener(mark, 'click', function(){ 
			mark.openExtInfoWindow(
				gmap,
				"market_window",
				"<div>This is a market</div>",
				{beakOffset: 0}
			); 
		});	
	});
}

function getNightlifePoints(){
    make_bits("nightlife", "/search/points_of_interest?type=nightlife&start=0&count=150", function(foo) {
        return new GMarker(new GLatLng(foo.latitude, foo.longitude), {'title': foo.name, 'icon': nightlifeIcon });
    }, 
	function(mark){
		GEvent.addListener(mark, 'click', function(){ 
			mark.openExtInfoWindow(
				gmap,
				"nightlife_window",
				"<div>This is a nightlife spot</div>",
				{beakOffset: 0}
			); 
		});	
	});
}

function getFitnessPoints(){
	getParks();
}

function getParks(){
	 make_bits("#parks", "/debug/parks", function(foo) {
        return makeline(foo[1], foo[2], "#00gg00", .4);
    }, 
	function(mark){
		GEvent.addListener(mark, 'click', function(){ 
			gmap.openInfoWindowHtml(mark.getBounds().getCenter(), 'this is a park', {maxWidth:300});
		});	
	});	
}

function hexify(n) {
    var s = Math.round(255*n).toString(16);
    console.log(n, s);
    return s.length == 1 ? "0" + s : s;
}

function makeline(border, levels, color, opacity) {
    return new GPolygon.fromEncoded({
        polylines: [{
            points: border,
            levels: levels,
            color: color,
            opacity: 1,
            weight: 4,
            numLevels: 4,
            zoomFactor: 32
        }],
        fill: true,
        color: color,
        opacity: opacity,
        outline: true
    });
}
/*-----------------------------------------------------------------*/
/* Local Search Functions */
function initLocalSearch(){
	searcher = new google.search.LocalSearch();
	searcher.setResultSetSize(google.search.Search.LARGE_RESULTSET);
	searcher.setCenterPoint(gmap);
	
	searcher.setSearchCompleteCallback(searcher , function() {    
    	var results = searcher.results; // Grab the results array
		var query = $("#localSearchInput").val();
		if(!isMapOverlay(query)){
			// We loop through to get the points
			for (var i = 0; i < results.length; i++) {
				var result = results[i]; // Get the specific result
				var markerLatLng = new google.maps.LatLng(parseFloat(result.lat),
														parseFloat(result.lng));
				var marker = new google.maps.Marker(markerLatLng); // Create the marker
		
				// Bind information for the infoWindow aka the map marker popup
				marker.bindInfoWindow(result.html.cloneNode(true));
				result.marker = marker; // bind the marker to the result
				storeMarker(query, marker);
				gmap.addOverlay(marker); // add the marker to the map
			}
	
			// Store where the map should be centered
			var center = searcher.resultViewport.center;
		
			// Calculate what the zoom level should be
			var ne = new google.maps.LatLng(searcher.resultViewport.ne.lat,
											searcher.resultViewport.ne.lng);
			var sw = new google.maps.LatLng(searcher.resultViewport.sw.lat,
											searcher.resultViewport.sw.lng);
			var bounds = new google.maps.LatLngBounds(sw, ne);
			var zoom = gmap.getBoundsZoomLevel(bounds);
		
			// Set the new center of the map
			// parseFloat converts the lat/lng from a string to a float, which is what
			// the LatLng constructor takes.
			gmap.setCenter(new google.maps.LatLng(parseFloat(center.lat),
												 parseFloat(center.lng)),
												 zoom);
			$("#localSearchInput").val('');
			$("#currentSearches").show();
			$("#currentSearches").append("<div class=\"tag ui-corner-all\"><a href=\"#\" class=\"ui-icon ui-icon-close floatLeft \" onClick=\"removeSearchResults(this);\"></a>" + query + "</div>");
			
		}
  	});
}

function localSearch(){
	if($("#currentSearches:visible").length == 0){
		$("#currentSearches").after('<br clear="all" />');
	}
	searcher.execute($("#localSearchInput").val());	
}

function removeSearchResults(obj){
	isMapOverlay($(obj).parent().text());
	$(obj).parent().remove();
	
	if($("#currentSearches .ui-corner-all").length == 0){
		$("#currentSearches").siblings('br').remove();
		$("#currentSearches").hide();
	}
}

/*-----------------------------------------------------------------*/
/* Rental Functions */
function addRentalsToMap(data){
	gmap.clearOverlays();
   
   	for(i = 0; i < data.length; i++){					    
		if(i == 0){
			var bounds = new GLatLngBounds(new GLatLng(data[i].fields.latitude, data[i].fields.longitude), new GLatLng(data[i].fields.latitude, data[i].fields.longitude));
		}													
		
		marker = createMarker(data[i].pk, new GLatLng(data[i].fields.latitude, data[i].fields.longitude), '<a href="/rental?id='+data[i].pk+'">'+data[i].fields.address1 + '<br >' + data[i].fields.city + ', ' + data[i].fields.state + ' ' + data[i].fields.zip + '</a>');
		rentalMarkers[data[i].fields.rental_id] = marker;
		gmap.addOverlay(marker);
		
		bounds.extend(new GLatLng(data[i].fields.latitude, data[i].fields.longitude));
   	}
   
	if(data.length == 1){
		bounds.extend(new GLatLng(parseFloat(data[0].fields.latitude) + .0025, parseFloat(data[0].fields.longitude) + .0025));
	   	bounds.extend(new GLatLng(parseFloat(data[0].fields.latitude) - .0025, parseFloat(data[0].fields.longitude) - .0025));
   	}
   
   gmap.setCenter(bounds.getCenter(), gmap.getBoundsZoomLevel(bounds));
}

function createMarker(rental_id, point, popuphtml) {
	var popuphtml = "<div id=\"popup\">" + popuphtml + "<\/div>";
	var marker = new GMarker(point, {'icon': apartmentIcon });
	GEvent.addListener(marker, "click", function() {
		marker.openExtInfoWindow(
			gmap,
			"rental_window",
			popuphtml,
			{beakOffset: 0}
		); 		
		if($("#resultsContainer").children().length > 0){
			$(".listing").removeClass("selected");
			$("#listing_"+rental_id).addClass("selected");
						
			// listing is invisible below viewport
			if($(".selected").position().top + $(".selected").height() > ($("#resultsContainer").height() + $("#resultsContainer").position().top )){								
				distFromTop = $(".selected").position().top - $("#resultsContainer").position().top + $(".selected").outerHeight();
				$("#resultsContainer").scrollTop((distFromTop - $("#resultsContainer").height()) + $("#resultsContainer").scrollTop());
			}
			// listing is invisible above viewport
			else if($(".selected").position().top < $("#resultsContainer").position().top){
				distFromTop = $(".selected").position().top - $("#resultsContainer").position().top;
				$("#resultsContainer").scrollTop($("#resultsContainer").scrollTop() + distFromTop);
			}
		}
	});
	return marker;
}

/*-----------------------------------------------------------------*/
/* Hood functions */
function addToMap(neighborhood_id, polygon){
	neighborhoodAreas[neighborhood_id] = polygon;
	gmap.addOverlay(polygon);
}

function removeFromMap(neighborhood_id){
	polygon = neighborhoodAreas[neighborhood_id];
	polygon.setStrokeStyle({opacity: 0});
	gmap.removeOverlay(polygon);
	delete neighborhoodAreas[neighborhood_id];
}

function isInMap(neighborhood_id){
	if(typeof(neighborhoodAreas[neighborhood_id]) !== 'undefined'){
		return true;
	}
	else{
		return false;
	}
}

function centerMapOnHood(data){
	//clear all outlines
	$.each(neighborhoodAreas, function(i, el){
		el.setStrokeStyle({weight: 0});
	});
	
	// cache the polygon if the user hasn't looked at it before
	if(!isInMap(data.neighborhood_id)){
		var polygon = makeline(data.encodedBorder, data.encodedLevels, data.color, .7);
		addToMap(data.neighborhood_id, polygon);
	}		
	else{
		var polygon	= neighborhoodAreas[data.neighborhood_id];
		polygon.setStrokeStyle({color: '#000000', weight: 3, opacity: 1});		
	}
	gmap.panTo(polygon.getBounds().getCenter(), 14);
	
}