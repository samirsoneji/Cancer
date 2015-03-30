(function($){
 $.fn.stoc = function(options) {
    //Our default options
    var defaults = {
        search: "body",        //where we will search for titles
        depth: 6,              //how many hN should we search
        start: 1,              //which hN will be the first (and after it we
                               //go just deeper)
        stocTitle: "Contents", //what to display before our box
        listType: "ul",        //could be ul or ol
        tocTitleEn: "Table of content for this section",
        tocTitleEs: "Tabla de contenidos para esta secci&#243;n",
        beforeText: "", // can add <span class="text-class">
        afterText: "", // can add </span> to match beforeText
        smoothScroll: 1
    };


    //let's extend our plugin with default or user options when defined
    var options = $.extend(defaults, options);

    // Select the language tag to pick the proper text for headings
    // TBD:  Are KeyPoints H3 or H4???
    // ------------------------------------------------------------
    if ($('meta[name="content-language"]').attr('content') == 'es')
       defaults.stocTitle = '<h3>' + defaults.tocTitleEs + '</h3>';
    else
       defaults.stocTitle = '<h3>' + defaults.tocTitleEn + '</h3>';

    // Need to identify if this is a HP or patient summary.  If it's a
    // patient summary we'll create KeyPoint boxes.
    // KeyPoint titles are H-tags with a type='keypoint' attribute
    if ( $("h3[type='keypoint']").length
           + $("h4[type='keypoint']").length > 0 ) {
        var kp = 1;
    }
    else {
        kp = 0;
    }


    return this.each(function() {
        //"cache" our target and search objects
        obj = $(this); //target
        src = $(options.search); //search

        // if container is not found.
        if (!src || 0 === src.length) {
            return;
        }

        //let's declare some variables. We need this var declaration to
        //create them as local variables (not global)
        var appHTML = "",
            tagNumber = 0,
            txt = "",
            id = "",
            beforeTxt = options.beforeText,
            afterTxt = options.afterText,
            previous = options.start,
            start = options.start,
            depth = options.depth,
            i = 0,
            srcTags = "h" + options.start,
            cacheHN = "";

        // Turn off the KeyPoint header (coming from the TOC box) (VE)
        if (kp == 1)
            options.stocTitle = ""

        //which tags we will search
        while ( depth > 1) {
            start++; //we will just get our start level and numbers higher than it
            srcTags = srcTags + ", h" + start;
            depth--; //since went one level up, our depth will go one level down
        }
        // if the target is not found
        // src.find(srcTags).each(function() {
        var found = src.find(srcTags);
        if (!found || 0 === found.length) {
            return;
        }

        found.each(function() {
            //we will cache our current H element
            cacheHN = $(this);
            //if we are on h1, 2, 3...
            tagNumber = ( cacheHN.get(0).tagName ).substr(1);

            //sets the needed id to the element
            //if it doesn't have one, of course
            // --------------------------------------------------
            id = cacheHN.attr('id');
            if (id == "" || typeof id === "undefined") {
                id = "stoc_h" + tagNumber + "_" + i;
                cacheHN.attr('id', id);
            }
            //our current text
            // using html() instead of text() since there could be markup
            txt = cacheHN.html();

            // Suppressing certain headings from TOC
            // The KeyPoint headings are only displayed in the KeyPoint
            // boxes (section level TOC) but not in the document
            // level TOC.  That means we'll have to suppress the KP
            // headings when searching on the body or article level but
            // need to include them at the section level.
            // Note:
            //   The prototype is using a different section-level
            //   structure.  A top-level section acts like an article
            //   and the search needs to be adjusted.
            // --------------------------------------------------------
            if (options.search == 'body' || options.search == 'article') {
                hAttr = cacheHN.attr("type");
            }
            else {
                hAttr = '';
            }

            // We also suppress headings for Reference sections and the
            // heading for the KP box itself.
            // ---------------------------------------------------------
            if (txt != 'References' 
                    && txt.substring(0, 14) != 'Key Points for'
                    && txt.substring(0, 10) != 'Bibliograf'
                    && txt.substring(0, 14) != 'Puntos importa'
                    && hAttr != 'keypoint'
                    && txt != 'Table of contents for this section') {
                switch(true) {                //with switch(true) we can do
                                              //comparisons in each case
                case (tagNumber > previous) : //it means that we went down
                                              //one level (e.g. from h2 to h3)
                        appHTML = appHTML + "<" + options.listType + ">"
                                          + "<li>"
                                          + beforeTxt
                                          + "<a href=\"#"+ id + "\">"
                                          + txt
                                          + "</a>";
                        previous = tagNumber;
                    break;
                case (tagNumber == previous) : //it means that stay on the
                                               //same level (e.g. h3 and
                                               //stay on it)
                        appHTML = appHTML + "</li>"
                                          + "<li>"
                                          + beforeTxt
                                          + "<a href=\"#" + id + "\">"
                                          + txt
                                          + "</a>";
                    break;
                case (tagNumber < previous) : //it means that we went up but
                                              //we don't know how much levels
                                              //(e.g. from h3 to h2)
                        while(tagNumber != previous) {
                            appHTML = appHTML + "</" + options.listType +">"
                                              + "</li>";
                            previous--;
                        }
                        appHTML = appHTML + "<li>"
                                          + beforeTxt
                                          + "<a href=\"#" + id + "\">"
                                          + txt
                                          + "</a>";
                    break;
                }
            }
            i++;
        });
        //corrects our last item, because it may have some opened ul's
        while(tagNumber != options.start && tagNumber > 0) {
            appHTML = appHTML + "</" + options.listType +">";
            tagNumber--;
        }

        // Clean up our Percussion workaround entry
        // We had to include text within the empty container div to prevent
        // Percussion from messing up the divs.  Setting text to blank now
        // ----------------------------------------------------------------
        if ( $("div.#_toc_article").length == 1
                                 && $("div.keyPoints").length == 0 ) {
            $("div.#_toc_section").text("");
            }

        //append our html to our object
        appHTML = options.stocTitle
                  + "<"+ options.listType + ">"
                  + appHTML
                  + "</" + options.listType + ">";

        // In the special case when we encounter a patient summary that
        // does contain citations (citations are only contained in HP
        // summaries except for one CAM summary) we need to suppress
        // the display of the TOC header without list items.
        emptyHTML = options.stocTitle
                  + "<"+ options.listType + ">"
                  + "</" + options.listType + ">";
        if ( appHTML != emptyHTML ) {
            obj.append(appHTML);
        }

        // SMOOTH SCROLL ------------------------------------------------
        // our pretty smooth scrolling here
        // acctually I've just compressed the code so you guys will think
        // that I'm the man.
        // Source: http://css-tricks.com/snippets/jquery/smooth-scrolling/
        if (options.smoothScroll == 1) {
            $(window).load(function(){
                function filterPath(string){return string.replace(/^\//,'').replace(/(index|default).[a-zA-Z]{3,4}$/,'').replace(/\/$/,'')}var locationPath=filterPath(location.pathname);var scrollElem=scrollableElement('html','body');obj.find('a[href*=#]').each(function(){var thisPath=filterPath(this.pathname)||locationPath;if(locationPath==thisPath&&(location.hostname==this.hostname||!this.hostname)&&this.hash.replace(/#/,'')){var $target=$(this.hash),target=this.hash;if(target){var targetOffset=$target.offset().top;$(this).click(function(event){event.preventDefault();$(scrollElem).animate({scrollTop:targetOffset},400,function(){location.hash=target})})}}});function scrollableElement(els){for(var i=0,argLength=arguments.length;i<argLength;i++){var el=arguments[i],$scrollElement=$(el);if($scrollElement.scrollTop()>0){return el}else{$scrollElement.scrollTop(1);var isScrollable=$scrollElement.scrollTop()>0;$scrollElement.scrollTop(0);if(isScrollable){return el}}}return[]}
            });
        }
    });
 };
})(jQuery);

