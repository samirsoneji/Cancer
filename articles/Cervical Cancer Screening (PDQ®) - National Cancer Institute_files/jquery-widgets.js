/* function to move government shutdown notice */
jQuery(document).ready(function($) {
$("#shutdown").insertAfter("#cgvMainNav");
});
/* function to move government shutdown notice */


/* BEGIN Yahoo style news slider, used on multimedia library on News and Events page */

/****************************************************************************
 Accessible News Slider
 
 courtesy:
 https://github.com/rip747/Yahoo-style-news-slider-for-jQuery
 
 Author:
 Brian Reindel, modified and adapted by Andrea Ferracani
 
 Author URL:
 http://blog.reindel.com, http://www.micc.unifi.it/ferracani
 
 License:
 Unrestricted. This script is free for both personal and commercial use.
 *****************************************************************************/

(function ($) {
    $.fn.accessNews = function (settings) {

        var defaults = {
            // title for the display
            title: "TODAY NEWS:",
            // subtitle for the display
            subtitle: "November 27 2010",
            // number of slides to advance when pagnating
            slideBy: 3,
            // the speed for the pagination
            speed: "normal",
            // slideshow interval
            slideShowInterval: 15000,
            // delay before slide show begins
            slideShowDelay: 15000,
            // theme
            theme: "business_as_usual"
        };

        return this.each(function () {

            settings = jQuery.extend(defaults, settings);
            var _this = jQuery(this);
            var stories = _this.children();
            var intervalId = null;

            var container = {

                _wrapper: "<div class=\"accessible_news_slider " + settings.theme + "\"></div>",
                _container: "<div class=\"container\"></div>",
                // We are not using the Title and Subtitle features
                /* _headline: jQuery("<div class='headline'></div>").html(["<p><strong>", settings.title, "</strong> ", settings.subtitle, "</p>"].join("")), */
                _content: jQuery("<div class='content'></div>"),
                _first: jQuery(stories[0]),

                init: function () {
                    // wrap the ul with our div class and assigned theme
                    _this.wrap(this._wrapper);
                    // our container where we show the image and news item
                    _this.before(this._container);
                    // set the width of the container
                    _this.css("width", (stories.length * this._first.width()));
                    this.append(this._headline);
                    this.append(this._content);
                    this.set(this._first);
                },

                append: function (content) {
                    this.get().append(content);
                },

                // returns the main container
                get: function () {
                    return _this.prev("div.container");
                },

                set: function (story) {
                    var container = this.get();
                    var _content = jQuery("div.content", container);
                    var img = jQuery('<img></img>');
                    var para = jQuery('<div></div>');
                    var title = jQuery('p.title a', story);
                    img.attr('src', jQuery('img', story).attr('src'));
                    title = title.attr('title') || title.text();
                    para.html("<h1>" + title + "</h1>" + "<p class='paraText'>" + jQuery('p.description', story).html() + "</p>");
                    stories.removeClass('selected');
                    story.addClass('selected');
                    _content.empty();
                    _content.append(img);
                    _content.append(para);
                }

            };

            var pagination = {

                loaded: false,
                _animating: false,
                _totalPages: 0,
                _currentPage: 1,
                _storyWidth: 0,
                _slideByWidth: 0,
                _totalWidth: 0,

                init: function () {
                    if (stories.length > settings.slideBy) {
                        this._totalPages = Math.ceil(stories.length / settings.slideBy);
                        this._storyWidth = jQuery(stories[0]).width();
                        this._slideByWidth = this._storyWidth * settings.slideBy;
                        this._totalWidth = this._storyWidth * stories.length;
                        this.draw();
                        this.loaded = true;
                    }
                },

                draw: function () {

                    var _viewAll = jQuery("<div class=\"view_all\"></div>").html(["<div class=\"count\"><span class=\"startAt\">1</span> - <span class=\"endAt\">", settings.slideBy, "</span> of ", stories.length, "</span></div><div class=\"controls\"><span class=\"back\"><a href=\"#\" title=\"Back\">&lt;&lt; Back</a></span><span class=\"next\"><a href=\"#\" title=\"Next\">Next &gt;&gt;</a></span></div>"].join(""));
                    _this.after(_viewAll);

                    var _next = jQuery(".next > a", _viewAll);
                    var _back = jQuery(".back > a", _viewAll);

                    _next.click(function () {

                        var page = pagination._currentPage + 1;
                        pagination.to(page);
                        return false;

                    });

                    _back.click(function () {

                        var page = pagination._currentPage - 1;
                        pagination.to(page);
                        return false;

                    });

                },

                to: function (page) {

                    if (this._animating) {
                        return;
                    }

                    var viewAll = _this.next(".view_all");
                    var startAt = jQuery(".startAt", viewAll);
                    var endAt = jQuery(".endAt", viewAll);

                    this._animating = true;

                    if (page >= this._totalPages) {
                        page = this._totalPages;
                    }

                    if (page <= 1) {
                        page = 1;
                    }

                    var _startAt = (page * settings.slideBy) - settings.slideBy;
                    var _left = parseInt(_this.css("left"));
                    var _offset = (page * this._slideByWidth) - this._slideByWidth;
                    startAt.html(_startAt + 1);
                    endAt.html(page * settings.slideBy);

                    _left = (_offset * -1);

                    _this.animate({
                        left: _left
                    }, settings.speed);

                    // when paginating set the active story to the first
                    // story on the page
                    container.set(jQuery(stories[_startAt]));

                    this._currentPage = page;
                    this._animating = false;

                }

            };

            var slideshow = {

                init: function () {
                    this.attach();
                    setTimeout(function () {
                        intervalId = "";
                        slideshow.on();
                    }, settings.slideShowDelay);
                },

                on: function () {
                    if (!intervalId) {
                        intervalId = setInterval(function () {
                            slideshow.slide();
                        }, settings.slideShowInterval);
                    }
                },

                off: function () {
                    if (intervalId) {
                        clearInterval(intervalId);
                        intervalId = null;
                    }
                },

                slide: function () {

                    var current = jQuery("li.selected", _this);
                    var next = current.next("li");
                    var page = 0;
                    var storyIndex = 0;
                    var storyMod = 0;

                    if (!next.length) {
                        next = jQuery(stories[0]);
                        page = 1;
                    }

                    container.set(next);

                    if (pagination.loaded) {
                        storyIndex = stories.index(next);
                        storyMod = (storyIndex) % settings.slideBy;

                        if (storyMod === 0) {
                            page = (Math.ceil(storyIndex / settings.slideBy)) + 1;
                        }

                        if (page > 0) {
                            pagination.to(page);
                        }
                    }
                },

                attach: function () {

                    var that = jQuery(_this).parent("div.accessible_news_slider");
                    that.hover(function () {
                        // pause the slideshow on hover
                        slideshow.off();
                    }, function () {
                        // resume slideshow on mouseout
                        slideshow.on();
                    });

                }

            };

            //setup the container
            container.init();
            // pagination setup
            pagination.init();
            // slideshow setup
            slideshow.init();
            // append hover every to each element to update container content
            stories.hover(function () {
                // set container contect to hovered li
                container.set(jQuery(this));
            }, function () {
                // do nothing
            });

        });
    };
})(jQuery); /* END Yahoo style news slider, used on multimedia library on News and Events page */


/* BEGIN Home Page Tile Slider */
/*
* Slides, A Slideshow Plugin for jQuery
* Intructions: http://slidesjs.com
* By: Nathan Searles, http://nathansearles.com
* Version: 1.1.4
* Updated: February 25th, 2011
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
* http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*/
(function ($) {
    $.fn.slides = function (option) {
        option = $.extend({}, $.fn.slides.option, option);
        return this.each(function () {
            $('.' + option.container, $(this)).children().wrapAll('<div class="slides_control"/>');
            var elem = $(this),
                control = $('.slides_control', elem),
                total = control.children().size(),
                width = control.children().outerWidth(),
                height = control.children().outerHeight(),
                start = option.start - 1,
                effect = option.effect.indexOf(',') < 0 ? option.effect : option.effect.replace(' ', '').split(',')[0],
                paginationEffect = option.effect.indexOf(',') < 0 ? effect : option.effect.replace(' ', '').split(',')[1],
                next = 0,
                prev = 0,
                number = 0,
                current = 0,
                loaded, active, clicked, position, direction, imageParent, pauseTimeout, playInterval;

            function animate(direction, effect, clicked) {
                if (!active && loaded) {
                    active = true;
                    option.animationStart(current + 1);
                    switch (direction) {
                    case 'next':
                        prev = current;
                        next = current + 1;
                        next = total === next ? 0 : next;
                        position = width * 2;
                        direction = -width * 2;
                        current = next;
                        break;
                    case 'prev':
                        prev = current;
                        next = current - 1;
                        next = next === -1 ? total - 1 : next;
                        position = 0;
                        direction = 0;
                        current = next;
                        break;
                    case 'pagination':
                        next = parseInt(clicked, 10);
                        prev = $('.' + option.paginationClass + ' li.current a', elem).attr('href').match('[^#/]+$');
                        if (next > prev) {
                            position = width * 2;
                            direction = -width * 2;
                        } else {
                            position = 0;
                            direction = 0;
                        }
                        current = next;
                        break;
                    }
                    if (effect === 'fade') {
                        if (option.crossfade) {
                            control.children(':eq(' + next + ')', elem).css({
                                zIndex: 10
                            }).fadeIn(option.fadeSpeed, option.fadeEasing, function () {
                                if (option.autoHeight) {
                                    control.animate({
                                        height: control.children(':eq(' + next + ')', elem).outerHeight()
                                    }, option.autoHeightSpeed, function () {
                                        control.children(':eq(' + prev + ')', elem).css({
                                            display: 'none',
                                            zIndex: 0
                                        });
                                        control.children(':eq(' + next + ')', elem).css({
                                            zIndex: 0
                                        });
                                        option.animationComplete(next + 1);
                                        active = false;
                                    });
                                } else {
                                    control.children(':eq(' + prev + ')', elem).css({
                                        display: 'none',
                                        zIndex: 0
                                    });
                                    control.children(':eq(' + next + ')', elem).css({
                                        zIndex: 0
                                    });
                                    option.animationComplete(next + 1);
                                    active = false;
                                }
                            });
                        } else {
                            control.children(':eq(' + prev + ')', elem).fadeOut(option.fadeSpeed, option.fadeEasing, function () {
                                if (option.autoHeight) {
                                    control.animate({
                                        height: control.children(':eq(' + next + ')', elem).outerHeight()
                                    }, option.autoHeightSpeed, function () {
                                        control.children(':eq(' + next + ')', elem).fadeIn(option.fadeSpeed, option.fadeEasing);
                                    });
                                } else {
                                    control.children(':eq(' + next + ')', elem).fadeIn(option.fadeSpeed, option.fadeEasing, function () {
                                        if ($.browser.msie) {
                                            $(this).get(0).style.removeAttribute('filter');
                                        }
                                    });
                                }
                                option.animationComplete(next + 1);
                                active = false;
                            });
                        }
                    } else {
                        control.children(':eq(' + next + ')').css({
                            left: position,
                            display: 'block'
                        });
                        if (option.autoHeight) {
                            control.animate({
                                left: direction,
                                height: control.children(':eq(' + next + ')').outerHeight()
                            }, option.slideSpeed, option.slideEasing, function () {
                                control.css({
                                    left: -width
                                });
                                control.children(':eq(' + next + ')').css({
                                    left: width,
                                    zIndex: 5
                                });
                                control.children(':eq(' + prev + ')').css({
                                    left: width,
                                    display: 'none',
                                    zIndex: 0
                                });
                                option.animationComplete(next + 1);
                                active = false;
                            });
                        } else {
                            control.animate({
                                left: direction
                            }, option.slideSpeed, option.slideEasing, function () {
                                control.css({
                                    left: -width
                                });
                                control.children(':eq(' + next + ')').css({
                                    left: width,
                                    zIndex: 5
                                });
                                control.children(':eq(' + prev + ')').css({
                                    left: width,
                                    display: 'none',
                                    zIndex: 0
                                });
                                option.animationComplete(next + 1);
                                active = false;
                            });
                        }
                    }
                    if (option.pagination) {
                        $('.' + option.paginationClass + ' li.current', elem).removeClass('current');
                        $('.' + option.paginationClass + ' li:eq(' + next + ')', elem).addClass('current');
                    }
                }
            }

            function stop() {
                clearInterval(elem.data('interval'));
            }

            function pause() {
                if (option.pause) {
                    clearTimeout(elem.data('pause'));
                    clearInterval(elem.data('interval'));
                    pauseTimeout = setTimeout(function () {
                        clearTimeout(elem.data('pause'));
                        playInterval = setInterval(function () {
                            animate("next", effect);
                        }, option.play);
                        elem.data('interval', playInterval);
                    }, option.pause);
                    elem.data('pause', pauseTimeout);
                } else {
                    stop();
                }
            }
            if (total < 2) {
                return;
            }
            if (start < 0) {
                start = 0;
            }
            if (start > total) {
                start = total - 1;
            }
            if (option.start) {
                current = start;
            }
            if (option.randomize) {
                control.randomize();
            }
            $('.' + option.container, elem).css({
                overflow: 'hidden',
                position: 'relative'
            });
            control.children().css({
                position: 'absolute',
                top: 0,
                left: control.children().outerWidth(),
                zIndex: 0,
                display: 'none'
            });
            control.css({
                position: 'relative',
                width: (width * 3),
                height: height,
                left: -width
            });
            $('.' + option.container, elem).css({
                display: 'block'
            });
            if (option.autoHeight) {
                control.children().css({
                    height: 'auto'
                });
                control.animate({
                    height: control.children(':eq(' + start + ')').outerHeight()
                }, option.autoHeightSpeed);
            }
            if (option.preload && control.find('img').length) {
                $('.' + option.container, elem).css({
                    background: 'url(' + option.preloadImage + ') no-repeat 50% 50%'
                });
                var img = control.find('img:eq(' + start + ')').attr('src') + '?' + (new Date()).getTime();
                if ($('img', elem).parent().attr('class') != 'slides_control') {
                    imageParent = control.children(':eq(0)')[0].tagName.toLowerCase();
                } else {
                    imageParent = control.find('img:eq(' + start + ')');
                }
                control.find('img:eq(' + start + ')').attr('src', img).load(function () {
                    control.find(imageParent + ':eq(' + start + ')').fadeIn(option.fadeSpeed, option.fadeEasing, function () {
                        $(this).css({
                            zIndex: 5
                        });
                        $('.' + option.container, elem).css({
                            background: ''
                        });
                        loaded = true;
                    });
                });
            } else {
                control.children(':eq(' + start + ')').fadeIn(option.fadeSpeed, option.fadeEasing, function () {
                    loaded = true;
                });
            }
            if (option.bigTarget) {
                control.children().css({
                    cursor: 'pointer'
                });
                control.children().click(function () {
                    animate('next', effect);
                    return false;
                });
            }
            if (option.hoverPause && option.play) {
                control.bind('mouseover', function () {
                    stop();
                });
                control.bind('mouseleave', function () {
                    pause();
                });
            }
            if (option.generateNextPrev) {
                var urlstring=document.location.href;
								if (urlstring.search("espanol") == -1) {
                $('.' + option.container, elem).after('<a href="#" class="' + option.prev + '"><img src="/PublishedContent/images/images/tile-slider-english-previous.gif" alt="View Previous Tile" border="0" width="110" height="15" /></a>');
                $('.' + option.prev, elem).after('<a href="#" class="' + option.next + '"><img src="/PublishedContent/images/images/tile-slider-english-next.gif" alt="View Next Tile" border="0" width="110" height="15" /></a>');
                }
                else {
                	$('.' + option.container, elem).after('<a href="#" class="' + option.prev + '"><img src="/PublishedContent/images/images/tile-slider-spanish-previous.gif" alt="View Previous Tile" border="0" width="110" height="15" /></a>');
                $('.' + option.prev, elem).after('<a href="#" class="' + option.next + '"><img src="/PublishedContent/images/images/tile-slider-spanish-next.gif" alt="View Next Tile" border="0" width="110" height="15" /></a>');
                }
                
                
            }
            $('.' + option.next, elem).click(function (e) {
                e.preventDefault();
                if (option.play) {
                    pause();
                }
                animate('next', effect);
            });
            $('.' + option.prev, elem).click(function (e) {
                e.preventDefault();
                if (option.play) {
                    pause();
                }
                animate('prev', effect);
            });
            if (option.generatePagination) {
                elem.append('<ul class=' + option.paginationClass + '></ul>');
                control.children().each(function () {
                    $('.' + option.paginationClass, elem).append('<li><a href="#' + number + '">' + (number + 1) + '</a></li>');
                    number++;
                });
            } else {
                $('.' + option.paginationClass + ' li a', elem).each(function () {
                    $(this).attr('href', '#' + number);
                    number++;
                });
            }
            $('.' + option.paginationClass + ' li:eq(' + start + ')', elem).addClass('current');
            $('.' + option.paginationClass + ' li a', elem).click(function () {
                if (option.play) {
                    pause();
                }
                clicked = $(this).attr('href').match('[^#/]+$');
                if (current != clicked) {
                    animate('pagination', paginationEffect, clicked);
                }
                return false;
            });
            $('a.link', elem).click(function () {
                if (option.play) {
                    pause();
                }
                clicked = $(this).attr('href').match('[^#/]+$') - 1;
                if (current != clicked) {
                    animate('pagination', paginationEffect, clicked);
                }
                return false;
            });
            if (option.play) {
                playInterval = setInterval(function () {
                    animate('next', effect);
                }, option.play);
                elem.data('interval', playInterval);
            }
        });
    };
    $.fn.slides.option = {
        preload: false,
        preloadImage: '/img/loading.gif',
        container: 'slides_container',
        generateNextPrev: false,
        next: 'next',
        prev: 'prev',
        pagination: true,
        generatePagination: true,
        paginationClass: 'pagination',
        fadeSpeed: 350,
        fadeEasing: '',
        slideSpeed: 350,
        slideEasing: '',
        start: 1,
        effect: 'slide',
        crossfade: false,
        randomize: false,
        play: 0,
        pause: 0,
        hoverPause: false,
        autoHeight: false,
        autoHeightSpeed: 350,
        bigTarget: false,
        animationStart: function () {},
        animationComplete: function () {}
    };
    $.fn.randomize = function (callback) {
        function randomizeOrder() {
            return (Math.round(Math.random()) - 0.5);
        }
        return ($(this).each(function () {
            var $this = $(this);
            var $children = $this.children();
            var childCount = $children.length;
            if (childCount > 1) {
                $children.hide();
                var indices = [];
                for (i = 0; i < childCount; i++) {
                    indices[indices.length] = i;
                }
                indices = indices.sort(randomizeOrder);
                $.each(indices, function (j, k) {
                    var $child = $children.eq(k);
                    var $clone = $child.clone(true);
                    $clone.show().appendTo($this);
                    if (callback !== undefined) {
                        callback($child, $clone);
                    }
                    $child.remove();
                });
            }
        }));
    };
})(jQuery); 
/* END Home Page Tile Slider */

/* BEGIN 508 Video Player */

/* this is the Google JSAPI Library, pulled here to be one consolidated file. More info: http://code.google.com/apis/ajax/documentation */
if (!window['google']) {
window['google'] = {};
}
if (!window['google']['loader']) {
window['google']['loader'] = {};
google.loader.ServiceBase = 'http://www.google.com/uds';
google.loader.GoogleApisBase = 'http://ajax.googleapis.com/ajax';
google.loader.ApiKey = 'notsupplied';
google.loader.KeyVerified = true;
google.loader.LoadFailure = false;
google.loader.Secure = false;
google.loader.ClientLocation = {"latitude":38.91,"longitude":-77.018,"address":{"city":"Washington","region":"DC","country":"USA","country_code":"US"}};
google.loader.AdditionalParams = '';
(function() {var e=true,f=null,h=false,i=encodeURIComponent,j=window,k=google,m=undefined,n=document;function o(a,b){return a.load=b}var p="push",q="length",r="prototype",s="setTimeout",t="replace",v="charAt",w="loader",x="substring",A="ServiceBase",B="name",C="getTime",D="toLowerCase";function E(a){if(a in F)return F[a];return F[a]=navigator.userAgent[D]().indexOf(a)!=-1}var F={};function G(a,b){var c=function(){};c.prototype=b[r];a.K=b[r];a.prototype=new c}
function H(a,b){var c=a.w||[];c=c.concat(Array[r].slice.call(arguments,2));if(typeof a.r!="undefined")b=a.r;if(typeof a.q!="undefined")a=a.q;var d=function(){var g=c.concat(Array[r].slice.call(arguments));return a.apply(b,g)};d.w=c;d.r=b;d.q=a;return d}function I(a){var b=new Error(a);b.toString=function(){return this.message};return b}function J(a,b){for(var c=a.split(/\./),d=j,g=0;g<c[q]-1;g++){d[c[g]]||(d[c[g]]={});d=d[c[g]]}d[c[c[q]-1]]=b}function K(a,b,c){a[b]=c}if(!L)var L=J;if(!aa)var aa=K;k[w].s={};L("google.loader.callbacks",k[w].s);var M={},N={};k[w].eval={};L("google.loader.eval",k[w].eval);
o(k,function(a,b,c){var d=M[":"+a];if(d){if(c&&!c.language&&c.locale)c.language=c.locale;if(c&&typeof c.callback=="string"){var g=c.callback;if(g.match(/^[[\]A-Za-z0-9._]+$/)){g=j.eval(g);c.callback=g}}var l=c&&c.callback!=f;if(l&&!d.p(b))throw I("Module: '"+a+"' must be loaded before DOM onLoad!");else if(l)d.k(b,c)?j[s](c.callback,0):d.load(b,c);else d.k(b,c)||d.load(b,c)}else throw I("Module: '"+a+"' not found!");});L("google.load",k.load);k.J=function(a,b){b?ba(a):O(j,"load",a)};
L("google.setOnLoadCallback",k.J);function O(a,b,c){if(a.addEventListener)a.addEventListener(b,c,h);else if(a.attachEvent)a.attachEvent("on"+b,c);else{var d=a["on"+b];a["on"+b]=d!=f?ca([c,d]):c}}function ca(a){return function(){for(var b=0;b<a[q];b++)a[b]()}}var P=[];
function ba(a){if(P[q]==0){O(j,"load",R);if(!E("msie")&&!(E("safari")||E("konqueror"))&&E("mozilla")||j.opera)j.addEventListener("DOMContentLoaded",R,h);else if(E("msie"))n.write("<script defer onreadystatechange='google.loader.domReady()' src=//:><\/script>");else(E("safari")||E("konqueror"))&&j[s](S,10)}P[p](a)}k[w].D=function(){var a=j.event.srcElement;if(a.readyState=="complete"){a.onreadystatechange=f;a.parentNode.removeChild(a);R()}};L("google.loader.domReady",k[w].D);var da={loaded:e,complete:e};
function S(){if(da[n.readyState])R();else P[q]>0&&j[s](S,10)}function R(){for(var a=0;a<P[q];a++)P[a]();P.length=0}
k[w].d=function(a,b,c){if(c){var d;if(a=="script"){d=n.createElement("script");d.type="text/javascript";d.src=b}else if(a=="css"){d=n.createElement("link");d.type="text/css";d.href=b;d.rel="stylesheet"}var g=n.getElementsByTagName("head")[0];g||(g=n.body.parentNode.appendChild(n.createElement("head")));g.appendChild(d)}else if(a=="script")n.write('<script src="'+b+'" type="text/javascript"><\/script>');else a=="css"&&n.write('<link href="'+b+'" type="text/css" rel="stylesheet"></link>')};
L("google.loader.writeLoadTag",k[w].d);k[w].G=function(a){N=a};L("google.loader.rfm",k[w].G);k[w].I=function(a){for(var b in a)if(typeof b=="string"&&b&&b[v](0)==":"&&!M[b])M[b]=new T(b[x](1),a[b])};L("google.loader.rpl",k[w].I);k[w].H=function(a){if((a=a.specs)&&a[q])for(var b=0;b<a[q];++b){var c=a[b];if(typeof c=="string")M[":"+c]=new U(c);else{var d=new V(c[B],c.baseSpec,c.customSpecs);M[":"+d[B]]=d}}};L("google.loader.rm",k[w].H);k[w].loaded=function(a){M[":"+a.module].i(a)};
L("google.loader.loaded",k[w].loaded);J("google_exportSymbol",J);J("google_exportProperty",K);function U(a){this.a=a;this.n={};this.b={};this.j=e;this.c=-1}
U[r].f=function(a,b){var c="";if(b!=m){if(b.language!=m)c+="&hl="+i(b.language);if(b.nocss!=m)c+="&output="+i("nocss="+b.nocss);if(b.nooldnames!=m)c+="&nooldnames="+i(b.nooldnames);if(b.packages!=m)c+="&packages="+i(b.packages);if(b.callback!=f)c+="&async=2";if(b.other_params!=m)c+="&"+b.other_params}if(!this.j){if(k[this.a]&&k[this.a].JSHash)c+="&sig="+i(k[this.a].JSHash);var d=[];for(var g in this.n)g[v](0)==":"&&d[p](g[x](1));for(g in this.b)g[v](0)==":"&&d[p](g[x](1));c+="&have="+i(d.join(","))}return k[w][A]+
"/?file="+this.a+"&v="+a+k[w].AdditionalParams+c};U[r].u=function(a){var b=f;if(a)b=a.packages;var c=f;if(b)if(typeof b=="string")c=[a.packages];else if(b[q]){c=[];for(var d=0;d<b[q];d++)typeof b[d]=="string"&&c[p](b[d][t](/^\s*|\s*$/,"")[D]())}c||(c=["default"]);var g=[];for(d=0;d<c[q];d++)this.n[":"+c[d]]||g[p](c[d]);return g};
o(U[r],function(a,b){var c=this.u(b),d=b&&b.callback!=f;if(d)var g=new W(b.callback);for(var l=[],u=c[q]-1;u>=0;u--){var y=c[u];d&&g.z(y);if(this.b[":"+y]){c.splice(u,1);d&&this.b[":"+y][p](g)}else l[p](y)}if(c[q]){if(b&&b.packages)b.packages=c.sort().join(",");if(!b&&N[":"+this.a]!=f&&N[":"+this.a].versions[":"+a]!=f&&!k[w].AdditionalParams&&this.j){var z=N[":"+this.a];k[this.a]=k[this.a]||{};for(var Q in z.properties)if(Q&&Q[v](0)==":")k[this.a][Q[x](1)]=z.properties[Q];k[w].d("script",k[w][A]+
z.path+z.js,d);z.css&&k[w].d("css",k[w][A]+z.path+z.css,d)}else if(!b||!b.autoloaded)k[w].d("script",this.f(a,b),d);if(this.j){this.j=h;this.c=(new Date)[C]();if(this.c%100!=1)this.c=-1}for(u=0;u<l[q];u++){y=l[u];this.b[":"+y]=[];d&&this.b[":"+y][p](g)}}});
U[r].i=function(a){if(this.c!=-1){X("al_"+this.a,"jl."+((new Date)[C]()-this.c),e);this.c=-1}for(var b=0;b<a.components[q];b++){this.n[":"+a.components[b]]=e;var c=this.b[":"+a.components[b]];if(c){for(var d=0;d<c[q];d++)c[d].C(a.components[b]);delete this.b[":"+a.components[b]]}}X("hl",this.a)};U[r].k=function(a,b){return this.u(b)[q]==0};U[r].p=function(){return e};function W(a){this.B=a;this.l={};this.o=0}W[r].z=function(a){this.o++;this.l[":"+a]=e};
W[r].C=function(a){if(this.l[":"+a]){this.l[":"+a]=h;this.o--;this.o==0&&j[s](this.B,0)}};function V(a,b,c){this.name=a;this.A=b;this.m=c;this.t=this.g=h;this.h=[];k[w].s[this[B]]=H(this.i,this)}G(V,U);o(V[r],function(a,b){var c=b&&b.callback!=f;if(c){this.h[p](b.callback);b.callback="google.loader.callbacks."+this[B]}else this.g=e;if(!b||!b.autoloaded)k[w].d("script",this.f(a,b),c);X("el",this[B])});V[r].k=function(a,b){return b&&b.callback!=f?this.t:this.g};V[r].i=function(){this.t=e;for(var a=0;a<this.h[q];a++)j[s](this.h[a],0);this.h=[]};
var Y=function(a,b){return a.string?i(a.string)+"="+i(b):a.regex?b[t](/(^.*$)/,a.regex):""};V[r].f=function(a,b){return this.F(this.v(a),a,b)};
V[r].F=function(a,b,c){var d="";if(a.key)d+="&"+Y(a.key,k[w].ApiKey);if(a.version)d+="&"+Y(a.version,b);var g=k[w].Secure&&a.ssl?a.ssl:a.uri;if(c!=f)for(var l in c)if(a.params[l])d+="&"+Y(a.params[l],c[l]);else if(l=="other_params")d+="&"+c[l];else if(l=="base_domain")g="http://"+c[l]+a.uri[x](a.uri.indexOf("/",7));k[this[B]]={};if(g.indexOf("?")==-1&&d)d="?"+d[x](1);return g+d};V[r].p=function(a){return this.v(a).deferred};
V[r].v=function(a){if(this.m)for(var b=0;b<this.m[q];++b){var c=this.m[b];if((new RegExp(c.pattern)).test(a))return c}return this.A};function T(a,b){this.a=a;this.e=b;this.g=h}G(T,U);o(T[r],function(a,b){this.g=e;k[w].d("script",this.f(a,b),h)});T[r].k=function(){return this.g};T[r].i=function(){};T[r].f=function(a,b){if(!this.e.versions[":"+a]){if(this.e.aliases){var c=this.e.aliases[":"+a];if(c)a=c}if(!this.e.versions[":"+a])throw I("Module: '"+this.a+"' with version '"+a+"' not found!");}var d=k[w].GoogleApisBase+"/libs/"+this.a+"/"+a+"/"+this.e.versions[":"+a][b&&b.uncompressed?"uncompressed":"compressed"];X("el",this.a);return d};
T[r].p=function(){return h};var ea=h,Z=[],fa=(new Date)[C](),X=function(a,b,c){if(!ea){O(j,"unload",ga);ea=e}if(c){if(!k[w].Secure&&(!k[w].Options||k[w].Options.csi===h)){a=a[D]()[t](/[^a-z0-9_.]+/g,"_");b=b[D]()[t](/[^a-z0-9_.]+/g,"_");var d="http://csi.gstatic.com/csi?s=uds&v=2&action="+i(a)+"&it="+i(b);j[s](H($,f,d),10000)}}else{Z[p]("r"+Z[q]+"="+i(a+(b?"|"+b:"")));j[s](ga,Z[q]>5?0:15000)}},ga=function(){if(Z[q]){$(k[w][A]+"/stats?"+Z.join("&")+"&nc="+(new Date)[C]()+"_"+((new Date)[C]()-fa));Z.length=0}},$=function(a){var b=
new Image,c=ha++;ia[c]=b;b.onload=b.onerror=function(){delete ia[c]};b.src=a;b=f},ia={},ha=0;J("google.loader.recordStat",X);J("google.loader.createImageForLogging",$);

}) ();google.loader.rm({"specs":["feeds",{"name":"books","baseSpec":{"uri":"http://books.google.com/books/api.js","ssl":null,"key":{"string":"key"},"version":{"string":"v"},"deferred":true,"params":{"callback":{"string":"callback"},"language":{"string":"hl"}}}},{"name":"friendconnect","baseSpec":{"uri":"http://www.google.com/friendconnect/script/friendconnect.js","ssl":null,"key":{"string":"key"},"version":{"string":"v"},"deferred":false,"params":{}}},"spreadsheets","gdata","visualization",{"name":"sharing","baseSpec":{"uri":"http://www.google.com/s2/sharing/js","ssl":null,"key":{"string":"key"},"version":{"string":"v"},"deferred":false,"params":{"language":{"string":"hl"}}}},"search",{"name":"maps","baseSpec":{"uri":"http://maps.google.com/maps?file\u003dgoogleapi","ssl":"https://maps-api-ssl.google.com/maps?file\u003dgoogleapi","key":{"string":"key"},"version":{"string":"v"},"deferred":true,"params":{"callback":{"regex":"callback\u003d$1\u0026async\u003d2"},"language":{"string":"hl"}}},"customSpecs":[{"uri":"http://maps.google.com/maps/api/js","ssl":null,"key":{"string":"key"},"version":{"string":"v"},"deferred":true,"params":{"callback":{"string":"callback"},"language":{"string":"hl"}},"pattern":"^(3|3..*)$"}]},"language","earth",{"name":"annotations","baseSpec":{"uri":"http://www.google.com/reviews/scripts/annotations_bootstrap.js","ssl":null,"key":{"string":"key"},"version":{"string":"v"},"deferred":true,"params":{"callback":{"string":"callback"},"language":{"string":"hl"},"country":{"string":"gl"}}}},"ads","elements"]});
google.loader.rfm({":feeds":{"versions":{":1":"1",":1.0":"1"},"path":"/api/feeds/1.0/8e09eed7fc0dd59c80503ea502548a85/","js":"default+en.I.js","css":"default.css","properties":{":JSHash":"8e09eed7fc0dd59c80503ea502548a85",":Version":"1.0"}},":search":{"versions":{":1":"1",":1.0":"1"},"path":"/api/search/1.0/d96605db404c4df12e9f4b815d8bf11e/","js":"default+en.I.js","css":"default.css","properties":{":JSHash":"d96605db404c4df12e9f4b815d8bf11e",":NoOldNames":false,":Version":"1.0"}},":language":{"versions":{":1":"1",":1.0":"1"},"path":"/api/language/1.0/1c7d3f9786a25ae9e8dfe368fb808a79/","js":"default+en.I.js","properties":{":JSHash":"1c7d3f9786a25ae9e8dfe368fb808a79",":Version":"1.0"}},":annotations":{"versions":{":1":"1",":1.0":"1"},"path":"/api/annotations/1.0/eed21f515e4557e7713a9eadbf24a941/","js":"default+en.I.js","properties":{":JSHash":"eed21f515e4557e7713a9eadbf24a941",":Version":"1.0"}},":earth":{"versions":{":1":"1",":1.0":"1"},"path":"/api/earth/1.0/2e6203e63ed613b9e55441aa9eb70e0a/","js":"default.I.js","properties":{":JSHash":"2e6203e63ed613b9e55441aa9eb70e0a",":Version":"1.0"}},":ads":{"versions":{":1":"1",":1.0":"1"},"path":"/api/ads/1.0/31f308c7bb13936126a472dbd588a671/","js":"default.I.js","properties":{":JSHash":"31f308c7bb13936126a472dbd588a671",":Version":"1.0"}}});
google.loader.rpl({":scriptaculous":{"versions":{":1.8.2":{"uncompressed":"scriptaculous.js","compressed":"scriptaculous.js"},":1.8.1":{"uncompressed":"scriptaculous.js","compressed":"scriptaculous.js"}},"aliases":{":1.8":"1.8.2",":1":"1.8.2"}},":yui":{"versions":{":2.6.0":{"uncompressed":"build/yuiloader/yuiloader.js","compressed":"build/yuiloader/yuiloader-min.js"},":2.7.0":{"uncompressed":"build/yuiloader/yuiloader.js","compressed":"build/yuiloader/yuiloader-min.js"}},"aliases":{":2":"2.7.0",":2.7":"2.7.0",":2.6":"2.6.0"}},":swfobject":{"versions":{":2.1":{"uncompressed":"swfobject_src.js","compressed":"swfobject.js"},":2.2":{"uncompressed":"swfobject_src.js","compressed":"swfobject.js"}},"aliases":{":2":"2.2"}},":ext-core":{"versions":{":3.0.0":{"uncompressed":"ext-core-debug.js","compressed":"ext-core.js"}},"aliases":{":3":"3.0.0",":3.0":"3.0.0"}},":mootools":{"versions":{":1.2.1":{"uncompressed":"mootools.js","compressed":"mootools-yui-compressed.js"},":1.2.2":{"uncompressed":"mootools.js","compressed":"mootools-yui-compressed.js"},":1.11":{"uncompressed":"mootools.js","compressed":"mootools-yui-compressed.js"}},"aliases":{":1":"1.11"}},":jqueryui":{"versions":{":1.7.2":{"uncompressed":"jquery-ui.js","compressed":"jquery-ui.min.js"},":1.6.0":{"uncompressed":"jquery-ui.js","compressed":"jquery-ui.min.js"},":1.7.0":{"uncompressed":"jquery-ui.js","compressed":"jquery-ui.min.js"},":1.7.1":{"uncompressed":"jquery-ui.js","compressed":"jquery-ui.min.js"},":1.5.3":{"uncompressed":"jquery-ui.js","compressed":"jquery-ui.min.js"},":1.5.2":{"uncompressed":"jquery-ui.js","compressed":"jquery-ui.min.js"}},"aliases":{":1.7":"1.7.2",":1":"1.7.2",":1.6":"1.6.0",":1.5":"1.5.3"}},":prototype":{"versions":{":1.6.0.2":{"uncompressed":"prototype.js","compressed":"prototype.js"},":1.6.0.3":{"uncompressed":"prototype.js","compressed":"prototype.js"}},"aliases":{":1":"1.6.0.3",":1.6":"1.6.0.3"}},":jquery":{"versions":{":1.2.3":{"uncompressed":"jquery.js","compressed":"jquery.min.js"},":1.3.1":{"uncompressed":"jquery.js","compressed":"jquery.min.js"},":1.3.0":{"uncompressed":"jquery.js","compressed":"jquery.min.js"},":1.3.2":{"uncompressed":"jquery.js","compressed":"jquery.min.js"},":1.2.6":{"uncompressed":"jquery.js","compressed":"jquery.min.js"}},"aliases":{":1":"1.3.2",":1.3":"1.3.2",":1.2":"1.2.6"}},":dojo":{"versions":{":1.2.3":{"uncompressed":"dojo/dojo.xd.js.uncompressed.js","compressed":"dojo/dojo.xd.js"},":1.3.1":{"uncompressed":"dojo/dojo.xd.js.uncompressed.js","compressed":"dojo/dojo.xd.js"},":1.1.1":{"uncompressed":"dojo/dojo.xd.js.uncompressed.js","compressed":"dojo/dojo.xd.js"},":1.3.0":{"uncompressed":"dojo/dojo.xd.js.uncompressed.js","compressed":"dojo/dojo.xd.js"},":1.2.0":{"uncompressed":"dojo/dojo.xd.js.uncompressed.js","compressed":"dojo/dojo.xd.js"}},"aliases":{":1":"1.3.1",":1.3":"1.3.1",":1.2":"1.2.3",":1.1":"1.1.1"}}});
}


  google.load("swfobject", "2.1");

//these functions are for the youtube player


function onYouTubePlayerReady(playerId) {
  ytplayer = document.getElementById("myytplayer");
  setInterval(updateytplayerInfo, 250);
  updateytplayerInfo();
  ytplayer.addEventListener("onStateChange", "onytplayerStateChange");
  ytplayer.addEventListener("onError", "onPlayerError");
}

 function onytplayerStateChange(newState) {
   setytplayerState(newState);
 }

 function onPlayerError(errorCode) {
   alert("An error occured: " + errorCode);
 }

 function updateytplayerInfo() {
   
 }

 // functions for the api calls
 function loadNewVideo(id, startSeconds) {
   if (ytplayer) {
     ytplayer.loadVideoById(id, parseInt(startSeconds));
   }
 }

 function cueNewVideo(id, startSeconds) {
   if (ytplayer) {
     ytplayer.cueVideoById(id, startSeconds);
   }
 }

 function play() {
   if (ytplayer) {
     ytplayer.playVideo();
   }
 }

 function pause() {
   if (ytplayer) {
     ytplayer.pauseVideo();
   }
 }

 function stop() {
   if (ytplayer) {
     ytplayer.stopVideo();
   }
 }

 function getPlayerState() {
   if (ytplayer) {
     return ytplayer.getPlayerState();
   }
 }

 function seekTo(seconds) {
   if (ytplayer) {
     ytplayer.seekTo(seconds, true);
   }
 }

 function getBytesLoaded() {
   if (ytplayer) {
     return ytplayer.getVideoBytesLoaded();
   }
 }

 function getBytesTotal() {
   if (ytplayer) {
     return ytplayer.getVideoBytesTotal();
   }
 }

 function getCurrentTime() {
   if (ytplayer) {
     return ytplayer.getCurrentTime();
   }
 }

 function getDuration() {
   if (ytplayer) {
     return ytplayer.getDuration();
   }
 }

 function getStartBytes() {
   if (ytplayer) {
     return ytplayer.getVideoStartBytes();
   }
 }

 function mute() {
   if (ytplayer) {
     ytplayer.mute();
   }
 }

 function unMute() {
   if (ytplayer) {
     ytplayer.unMute();
   }
 }
 
 function getEmbedCode() {
   alert(ytplayer.getVideoEmbedCode());
 }

 function getVideoUrl() {
   alert(ytplayer.getVideoUrl());
 }
 
 function setVolume(newVolume) {
   if (ytplayer) {
     ytplayer.setVolume(newVolume);
   }
 }

 function getVolume() {
   if (ytplayer) {
     return ytplayer.getVolume();
   }
 }

 function clearVideo() {
   if (ytplayer) {
     ytplayer.clearVideo();
   }
 }


/* END 508 Video Player */


/* BEGIN Cancer Bulletin Science Shots */
var state = 'none';
function showhide(layer_ref) {
if (state == 'block') { 
state = 'none'; 
} 
else { 
state = 'block'; 
} 
if (document.all) { //IS IE 4 or 5 (or 6 beta) 
eval( "document.all." + layer_ref + ".style.display = state"); 
} 
if (document.layers) { //IS NETSCAPE 4 or below 
document.layers[layer_ref].display = state; 
} 
if (document.getElementById) {
if (!document.all) { 
hza = document.getElementById(layer_ref); 
hza.style.display = state; 
}
}
}
/* END Cancer Bulletin Science Shots */

/* BEGIN TCZ Toggle */

$(function(){

/* Make sure the timely content zone and its required data are
   present.  Otherwise, don't try to set it up. */
var langData = $('meta[name="content-language"]');
var tczToggle = $('#tcz-toggle');
var tcz508 = $('#tcz508');
var newsSlider = $('#news-slider');

if( tczToggle == null || tcz508 == null || newsSlider == null || langData == null )
	return;

var lang = langData.attr('content');

	if(lang == "en"){
  			tczToggle.text("View All News Items")
  			tczToggle.click(function(){
     			newsSlider.toggle();
     		tcz508.toggle();
  			});
  
  			tczToggle.toggle(function (){
    			$(this).text("Close News Items")
			}, function(){
    			$(this).text("View All News Items")
			});
		}
		
	else{ 
  			tczToggle.text("Vea todas las notas")
  			tczToggle.click(function(){
     			newsSlider.toggle();
     			tcz508.toggle();
  			});

  			tczToggle.toggle(function (){
    			$(this).text("Cierre las notas de la página")
			}, function(){
    			$(this).text("Vea todas las notas")
			});
	}
});

/* END TCZ Toggle */
 
/* function to remove unwanted break tag from page after title */
jQuery(document).ready(function($) {
$("div#cgvBody > div > br:first-child, #cgvBody > div > h1 + br").remove();
});
/* end function to remove unwanted break tag from page after title */
