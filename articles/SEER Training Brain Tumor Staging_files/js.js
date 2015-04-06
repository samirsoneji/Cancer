// JavaScript Document

function _ims_templates_onload() {

		var code_to_run = [];

		var add = function ( c ) {
			code_to_run.push( c );
		}
		var runall = function ( ) {
			for(var i=0; i < code_to_run.length; i++) {
				if( typeof(code_to_run[i])=='function')
					code_to_run[i]();
				else {
					try { eval(code_to_run[i]); }
					catch( exc ) { 'Note: some error occurred!'; }
				}
			}
		}

		this.add = add;
		this.run = runall;
		this.code_to_run = code_to_run;
}
var IMS_Onload = new _ims_templates_onload();

var newWinGlossary = null;
function openWinGlossary(pageUrl) {
	var baseUrl = "";
	if( pageUrl != "" ) {
		try {
			if (newWinGlossary) newWinGlossary.close();
			newWinGlossary = window.open(pageUrl,"newWinGlossary","height=190px,width=575px,toolbar=0,scrollbars,resizable");
			newWinGlossary.focus();
		}
		catch(e) { alert("Error: " + e ); }
	}
	return "";
}

var newWin = null;
function openWin(pageUrl ) {
	var baseUrl = "";
	if( pageUrl != "" ) {
		try {
			//if (newWin) newWin.close();
			if (!newWin || newWin.closed)
				newWin = window.open(pageUrl,"newWin","height=500px,width=750px,toolbar=0,scrollbars,resizable");
			else
				newWin.location=pageUrl;
			newWin.focus();
		}
		catch(e) { alert("Error: " + e ); }
	}
	return "";
}

var newWinCustom = null;
function openWinCustom( pageUrl, optionsString ) {
	var baseUrl = "";
	if( pageUrl != "" ) {
		try {
			if (!newWinCustom || newWinCustom.closed)
				newWinCustom = window.open(pageUrl,"newWinCustom",optionsString);
			else
				newWinCustom.location=pageUrl;
			newWinCustom.focus();
		}
		catch(e) { alert("Error: " + e ); }
	}
	return "";
}

