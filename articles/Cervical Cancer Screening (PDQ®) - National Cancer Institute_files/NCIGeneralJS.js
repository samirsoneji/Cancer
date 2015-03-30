/* FILE : NCIGeneralJS.js 

	PURPOSE: Scripts in this file should be for use on all sites hosted by the Percussion CMS
*/

// Exit Disclaimer Adder
//  This script looks for urls where the href points to websites not in the federal domain (.gov) and if it finds one, it appends an image to the link.  The image itself links to the exit disclaimer page.

//The three tests in the filter fuction test for the following criteria
// !/https?\:\/\/([a-zA-Z0-9\-]+\.)+gov/.test(this.href) : The href is a valid url that does not end in .gov
jQuery(document).ready(function($) {
//document.write($('meta').name);
var path;
var altText;
var lang = $('meta[name="content-language"]').attr('content');
if (!path){
	if( lang == "en"){
		path = $('meta[name="english-linking-policy"]').attr('content');
		altText ='Exit Disclaimer';
		}
	else{ 
		path = $('meta[name="espanol-linking-policy"]').attr('content');
		altText ='Notificaci\u00F3n de salida';
		}
}
$("a").filter(function () {  return /^https?\:\/\/([a-zA-Z0-9\-]+\.)+/.test(this.href) && !/^https?\:\/\/([a-zA-Z0-9\-]+\.)+gov/.test(this.href) && this.href != "" && this.href.indexOf(location.protocol +"//" +location.hostname) != 0 && !$(this).hasClass("add_this_btn") && !$(this).hasClass("no-exit-notification") }).after(' <a class="exitNotification" href=' + path + '><img title='+ '"' + altText +'"' +'  alt='+ '"' + altText +'"' + ' src="/publishedcontent/images/images/exit_small.png" /></a>');
if($('.with-image').length == 0){
    $('.list-spacer-image').addClass('image-collapsed');
	$('.list-item-with-image').addClass('text-collapsed');
}

/* add nopin tag to images that we do not want used on Pinterest */
$( "#cgvSiteBanner img, #cgvFooter img, .leftzone img, .news-slider-tabs img" ).attr( "nopin", "nopin" );

/*code to enlarge an image or a table*/
if($(".expandable-container").length > 0){

  $(".expandable-container").supersizeme( { } );  
}
  
});
