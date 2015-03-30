// add the "js" class to the body to use as a hook for progressive enhancement
$("body").addClass("js");

// initialize the SmartMenu navigation
$(function() {
	$('#main-menu').smartmenus();
	// look for dropdowns, add ARIA support
	$(".has-submenu").attr("aria-haspopup", "true");
	$("#main-menu li ul li a").attr("tabindex", "-1");
});


// testing window size to run certain functions
// this method is needed since IE8 window.resize fires when ANY element is resized, not just the viewport
var lastWindowHeight = $(window).height();
var lastWindowWidth = $(window).width();
$(window).resize(function () {
	 //confirm window was actually resized
     if($(window).height()!=lastWindowHeight || $(window).width()!=lastWindowWidth){

		//set this windows size
		lastWindowHeight = $(window).height();
		lastWindowWidth = $(window).width();

		// change the order of the DOM on resize so that the left navigation column comes after the content column on mobile (accessibility and source order for screen readers)
		// pixel amount will vary depending on your breakpoint
		var width = window.innerWidth || document.documentElement.clientWidth || document.body.clientWidth;	
		if ( width < 992) {
			$("#leftCol").insertAfter($("#skipTo"));
		} else {
			$("#leftCol").insertBefore($("#skipTo"));
		}
		
		// hiding the menu on small viewports and adding ARIA support, then revealing again and removing ARIA support on larger viewport
		// pixel amount will vary depending on your breakpoint
		//if ( width < 768) {
			//$("#main-menu").attr({ "aria-expanded": "false", "role": "region", "aria-labeledby": "menu-label", "tabindex": "-1" }).hide();
		//} else {
			//$("#main-menu").removeAttr("aria-expanded role region aria-labeledby tabindex").show();
		//}
	 }
});

// the previous function ran on window resize, we need one that runs on page load to catch mobile users	 
$(document).ready(function() {
	var width = window.innerWidth || document.documentElement.clientWidth || document.body.clientWidth;	
	if ( width < 992 ) {
		$("#leftCol").insertAfter($("#skipTo"));
	} 
	if ( width < 100 ) {
		$("#main-menu").attr({ "aria-expanded": "false", "role": "region", "aria-labeledby": "menu-label", "tabindex": "-1" }).hide();
	}

	// functionality for menu toggle on smaller viewports
	// check the state of the ARIA role, show or hide depending on that state, update the ARIA role if necessary
	$(".navToggle").click(function() {
		if ( $("#main-menu").attr("aria-expanded") == "false" ) {
			$("#main-menu").slideDown().attr("aria-expanded", "true");
			$(".navToggle span").addClass("active");
			$(".navToggle").attr({ "aria-pressed": "true", "alt": "Menu Expanded" });
		} else if ( $("#main-menu").attr("aria-expanded") == "true" ){
			$("#main-menu").slideUp().attr("aria-expanded", "false");
			$(".navToggle span").removeClass("active");
			$(".navToggle").attr({ "aria-pressed": "false", "alt": "Menu Collapsed" });
		}
		return false;
	});
	
	// check for clicks outside of mobile navigation, if user clicks outside, hide the navigation
	$(document).click(function() {
		// cheap way of making sure we are in mobile view: check for the mobile nav bar element that is only shown in mobile
		// if we don't do this, the nav gets hidden in desktop view too
		if ( $(".mobileNav").css("display") == "block") {
			$("#main-menu").slideUp().attr("aria-expanded", "false");
			$(".navToggle span").removeClass("active");
			$(".navToggle").attr({ "aria-pressed": "false", "alt": "Menu Collapsed" });
		} 
	});

	$("#main-menu").click(function(event) {
		event.stopPropagation();
	});
	
	$( ".has-submenu" ).focus(function() {
    	$("#main-menu li ul li a").attr("tabindex", "0");
 	});
});

