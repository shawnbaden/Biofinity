var geocoder;
var elevationService;

function lookupLocation() {
	var latitude = $("#location-latitude").val();
	var longitude = $("#location-longitude").val();
	var location = new google.maps.LatLng(latitude, longitude);
	geocoder = new google.maps.Geocoder();
	geocoder.geocode(
		{
			'latLng': location
		},
		function(results, status) {
			if (status == google.maps.GeocoderStatus.OK) {
				for (var component in results[0]['address_components']) {
					for (var i in results[0]['address_components'][component]['types']) {
						var componentType = results[0]['address_components'][component]['types'][i];
						var componentValue = results[0]['address_components'][component]['long_name'];
						if ("country" == componentType && $("#location-country").length) {
							$("#location-country").val(componentValue);
							$("#location-country")[0].onblur();
						}
						if ("administrative_area_level_2" == componentType && $("#location-county").length) {
							$("#location-county").val(componentValue);
							$("#location-county")[0].onblur();
						}
						if ("premise" == componentType && $("#location-locality").length) {
							$("#location-locality").val(componentValue);
							$("#location-locality")[0].onblur();
						}
						if ("locality" == componentType && $("#location-municipality").length) {
							$("#location-municipality").val(componentValue);
							$("#location-municipality")[0].onblur();
						}
						if ("administrative_area_level_1" == componentType && $("#location-state-province").length) {
							$("#location-state-province").val(componentValue);
							$("#location-state-province")[0].onblur();
						}
					}
				}
			} else {
				alert("The Google service used to determine location is not responding.");
			}
		}
	);
	elevationService = new google.maps.ElevationService();
	var locations = [];
	locations.push(location);
	elevationService.getElevationForLocations(
		{
			"locations": locations
		},
		function(results, status) {
			if (status == google.maps.ElevationStatus.OK) {
				$("#location-verbatim-elevation").val(results[0].elevation);
				$("#location-verbatim-elevation")[0].onblur();
			} else {
				alert("The Google service used to determine location is not responding.");
			}
		}
	);
}