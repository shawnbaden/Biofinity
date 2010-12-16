function globalLoad() {
	$("a[rel='colorboximage']").colorbox();
}

function insertScript(targetURL)
{
	var scriptElement = document.createElement("script");
	scriptElement.type="text/javascript";
	scriptElement.src = targetURL;
	document.getElementsByTagName("head")[0].appendChild(scriptElement);
}

function initDatePicker(elementID) {
	$("#" + elementID).datepicker({
		onSelect: function(dateText, inst) {
			var dateElement = document.getElementById(elementID);
			dateElement.onblur();
		}
	});
}

var actopts_visible = false;
function toggleAccountOptions() {
	$("#labmenuoptions").hide();
	labopts_visible = false;
	if ( actopts_visible ) {
		$("#account_options").hide();
		actopts_visible = false;
	} else {
		$("#account_options").show();
		actopts_visible = true;
	}
}

var labopts_visible = false;
function toggleLabOptions() {
	$("#account_options").hide();
	actopts_visible = false;
	if ( labopts_visible ) {
		$("#labmenuoptions").hide();
		labopts_visible = false;
	} else {
		$("#labmenuoptions").show();
		labopts_visible = true;
	}
}