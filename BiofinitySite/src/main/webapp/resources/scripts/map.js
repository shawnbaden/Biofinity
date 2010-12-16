var map;
var markerClusterer;
var activeMarker;
var markers = [];
var markerIDs = [];
var colors = ["00C100", "0000B8", "F1D700", "D700D7", "B91E00", "0080FF", "CF9494", "EA3511", "49D470", "8084A2", "715F2D", "FFFFFF"];
var colorIndex = -1;
var colorMap = {};
var infoWindow = null;
var infoWindowContent = "";

function displayMap(latitude, longitude, zoom) {
    var position = new google.maps.LatLng(latitude, longitude);
    var mapOptions = {
      center: position,
      mapTypeId: google.maps.MapTypeId.HYBRID
    };
    map = new google.maps.Map(document.getElementById("map_canvas"), mapOptions);
    infoWindow = new google.maps.InfoWindow();
    google.maps.event.addListener(
    	map,
    	"click",
    	function() {
			if (infoWindow) {
				infoWindow.close();
			}
		}
	);
    map.setZoom(zoom);
}

function setBounds(north, east, south, west) {
	var northEast = new google.maps.LatLng(north, east);
	var southWest = new google.maps.LatLng(south, west);
	
	var bounds = new google.maps.LatLngBounds(southWest, northEast);
	
	map.fitBounds(bounds);
	map.setCenter(bounds.getCenter());
}

function setZoom(zoom) {
	map.setZoom(zoom);
}

function createMarker(latitude, longitude, title, targetURL, showInfoWindow, draggable) {
	var marker = new StyledMarker({
		position: new google.maps.LatLng(latitude, longitude),
		styleIcon:new StyledIcon(StyledIconTypes.MARKER, {color:"00C100"})
	});
	marker.setDraggable(draggable);
	marker.setTitle(title);
	if (null != targetURL && "" != targetURL) {
		google.maps.event.addListener(
			marker,
			"click",
			function() {
				window.document.location.href = targetURL;
			}
		);
	} else if (showInfoWindow) {
		google.maps.event.addListener(
			marker,
			'click',
			function() {
				var markerIndex = markers.indexOf(marker);
				if (-1 < markerIndex) {
					var markerID = markerIDs[markerIndex];
					if (window.setInfoWindowContent) {
						activeMarker = marker;
						infoWindowContent = "";
						setInfoWindowContent(markerID);
					}
				}
			}
		);
	}
	if (draggable) {
		google.maps.event.addListener(
			marker,
			"dragend",
			function() {
				if (window.updateLocation) {
					updateLocation(String(activeMarker.getPosition().lat()), String(activeMarker.getPosition().lng()));
				}
				map.setCenter(activeMarker.getPosition());
			}
		);
	}
	
	return marker;
}

function addMarker(latitude, longitude, title, targetURL, showInfoWindow, draggable, centerMap) {
	var marker = createMarker(latitude, longitude, title, targetURL, showInfoWindow, draggable);
	if (centerMap) {
		map.setCenter(marker.getPosition(), 5);
	}
	marker.setMap(map);
	activeMarker = marker;
	
	return marker;
}

function moveMarker(latitude, longitude) {
	if (null != activeMarker) {
		activeMarker.setPosition(new google.maps.LatLng(latitude, longitude));
		map.setCenter(activeMarker.getPosition());
	}
}

function openMarkerInfoWindow() {
	infoWindow.setContent(infoWindowContent);
	infoWindow.open(map, activeMarker);
}

function addMarkers(markersJSON, clearMarkers) {
	if (clearMarkers) {
		resetMarkers();
	}
	var maximumLatitude  = -90.0;
	var maximumLongitude = -180.0;
	var minimumLatitude   = 90.0;
	var minimumLongitude  = 180.0;
	for (var key in markersJSON) {
		var markerJSON = markersJSON[key];
		var marker = createMarker(markerJSON.latitude, markerJSON.longitude, markerJSON.title, markerJSON.targetURL, true, false);
		colorIndex = (colorIndex + 1) % colors.length;
		var color = colors[colorIndex];
		if (typeof colorMap[markerJSON.title] == "undefined") {
			colorMap[markerJSON.title] = color;
		}
		marker.styleIcon.set("color", colorMap[markerJSON.title]);
		markers.push(marker);
		markerIDs.push(markerJSON.ID);
		if (parseFloat(markerJSON.latitude) > maximumLatitude) {
			maximumLatitude = parseFloat(markerJSON.latitude);
		}
		if (parseFloat(markerJSON.longitude) > maximumLongitude) {
			maximumLongitude = parseFloat(markerJSON.longitude);
		}
		if (parseFloat(markerJSON.latitude) < minimumLatitude) {
			minimumLatitude = parseFloat(markerJSON.latitude);
		}
		if (parseFloat(markerJSON.longitude) < minimumLongitude) {
			minimumLongitude = parseFloat(markerJSON.longitude);
		}
	}
	if (maximumLatitude != -90.0 && maximumLongitude != -180.0 && minimumLatitude != 90.0 && minimumLongitude != 180.0) {
		setBounds(maximumLatitude, maximumLongitude, minimumLatitude, minimumLongitude);
	}
	markerClusterer = new MarkerClusterer(map, markers, { gridSize: 25, maxZoom: 8 });
}

function resetMarkers() {
	if (null != markerClusterer) {
		markerClusterer.clearMarkers();
	}
	for (i in markers) {
		markers[i].setMap(null);
	}
	markers = [];
	markerIDs = [];
	colorIndex = -1;
	sytleIcons = {};
}