// (function($) {})  --> Default jQuery structure for functions
// Everything goes within '{}'
//
// Function to create an "Enlarge" button to click in order to 
// display a table or image using the entire window area
// --------------------------------------------------------------
(function($) {
    $.fn.supersizeme = function( options ) {
        // Adding some default settings
        var settings = $.extend({
            text: "Default Text",
            color: null
        }, options); 

    // Select the language tag to pick the proper text for headings
    // TBD:  Are KeyPoints H3 or H4???
    // ------------------------------------------------------------
    var enlargeTxt = "Enlarge";
    var collapseTxt = "Collapse";
    if ($('meta[name="content-language"]').attr('content') == 'es') {
       //defaults.stocTitle = '<h3>' + defaults.text + '</h3>';
       enlargeTxt = "Ampliar";
       collapseTxt = "Reducir";
       }

    // Creating the "Enlarge" link above the table or figure
    // -----------------------------------------------------------
    return this.each( function() { 
            //"cache" our target and search objects
		    myObj = $(this); //target

            // Create the anchor link with the link text and add it
            // before the selected element (table or figure)
            // ----------------------------------------------------
            var linkLabel = "<a href='javascript:void(0)'>" + 
                             enlargeTxt + "</a>";

            // Adding the Enlarge button 
            myObj.before( linkLabel );

            // Adding the scrollbar div for tables.
            // Also adding a (unnecessary) div for figures so that it's
            // simpler to handle both, figures and tables with the same
            // code
            // ---------------------------------------------------------
            if ( myObj[0].tagName == 'FIGURE' ) {
                myObj.wrap("<div class='pdq-figure'></div>"); 
            }
            else {
                myObj.wrap("<div class='pdq-table-scrollbar'></div>"); 
            }

            var objectID = myObj.attr("id") + "_e";
            myObj.parent()
                 .prev("a")
                 .attr("id", objectID )
                 .attr("href", "#" + objectID)
                 .addClass("pdq-link-enlarge");

            // Add the "normal" class to each element to indicate
            // an element *not* being enlarged
            // --------------------------------------------------
            myObj.parent()
                 .addClass("normal");

            // Create the click event on the Enlarge link
            // -------------------------------------------
            $("a.pdq-link-enlarge").unbind().click(function() {
                // Clicking on the 'Enlarge' link
                if ( $(this).next().hasClass("normal") ) {
                    // Make sure no other object is enlarged
                    $( "#_spacer").detach();

                    // Close all other open images (only one should exist)
                    if ( $(".pdq-image-table-enlarge").length > 0 ) {
                        $(".pdq-image-table-enlarge")
                                 .removeClass("pdq-image-table-enlarge")
                                 .removeClass("enlarged")
                                 .addClass("normal");
                        $("a.pdq-link-enlarge").text(enlargeTxt);
                    }

                    // Create our shim, enlarge the element and move
                    // all text down by inserting the shim.
                    // Finally, change the button text.
                    // ---------------------------------------------
                    var addTShim = "<div id='_spacer'></div>";

                    // Compute the height of the old table + padding
                    var eHght = $( this ).height();

                    $( this ).next()
                             .removeClass("normal")
                             .addClass("pdq-image-table-enlarge")
                             .addClass("enlarged");
                             //.css("background-color", "pink");
                    $( this ).next()
                             .after(addTShim);
                    $( this ).text(collapseTxt);

                    // The class pdq-image-table-enlarge works well for
                    // large tables but what if the enlarged table isn't 
                    // really that big?  Check if the table needs 
                    // to be centered because its size is smaller than the
                    // window size.
                    var winSize = $( window ).width();

                    var tabSize = $( this ).next().children("table").width();
                    var figSize = $( this ).next().children("figure").width();

                    var tabLeft = (winSize - tabSize) / 2;
                    var figLeft = (winSize - figSize) / 2;

                    if (tabLeft < 50) {
                        // do nothing; keep the predefined class definition
                    }
                    else {
                        // Adjusting CSS for small tables
                        $( this ).next().css("left", tabLeft)
                                        .css("right", tabLeft);
                    }

                    // Adjusting CSS for Figures
                    if ( $( this ).next().children("figure").length > 0 ) {
                        $( this ).next().css("left", figLeft)
                                        .css("right", figLeft);
                        $( this ).next().children("figure")
                                 .css("width", "100%");
                    }


                    // The table could become shorter when enlarging
                    // takes place due to changing line breaks
                    // We need to adjust the table shim if that happens
                    // or just set it to the new table height.
                    // Note: The caption and margins are not counted in 
                    //       the table height, so we need some padding.
                    var nHght = $( this ).next()
                                            .outerHeight();
                    var nCapHght = $( this ).next()
                                            .find("caption")
                                            .height();
                    var nDivHght = nHght + nCapHght + 10;
                    $( "#_spacer").height(nDivHght);
                }
                // Clicking on the 'Collapse' link
                else {
                    $( "#_spacer").detach();

                    $( this ).next()
                             .removeClass("pdq-image-table-enlarge")
                             .css("width", "")
                             .css("right", "")
                             .css("left", "")
                             .removeClass("enlarged")
                             .addClass("normal");
                    $( this ).text(enlargeTxt);
                    $( this ).next().find("figure").css("width", "");
                }
            });    

        });
    };

}(jQuery));
