/*! Responsive v3.1.1 | MIT License | responsivebp.com */
!function(t,e,i){"use strict";t.pseudoUnique=function(t){var e=t||8,i="",n="ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789",o=n.length;e>o&&(e=o);for(var s=0;e>s;s+=1)i+=n.charAt(Math.floor(Math.random()*o));return i},t.support.rtl=function(){return t("html[dir=rtl]").length?!0:!1}(),t.support.currentGrid=function(){var e=t("<div/>").addClass("grid-state-indicator").prependTo("body");return function(){var t=["xs","s","m","l"],i=parseInt(e.width(),10);return{grid:t[i],index:i,range:t}}}(),t.support.transition=function(){var t=function(){var t=i.createElement("div"),e={transition:"transitionend",WebkitTransition:"webkitTransitionEnd",MozTransition:"transitionend",OTransition:"oTransitionEnd otransitionend"};for(var n in e)if(void 0!==t.style[n])return{end:e[n]};return!1};return t()}(),t.fn.redraw=function(){var t;return this.each(function(){t=this.offsetWidth})},t.fn.ensureTransitionEnd=function(i){var n=/\d+(.\d+)/,o=!1,s=t(this),a=function(){o||s.trigger(t.support.transition.end)};return i||(i=1e3*(n.test(s.css("transition-duration"))?s.css("transition-duration").match(n)[0]:0)),s.one(t.support.transition.end,function(){o=!0}),e.setTimeout(a,i),this},t.fn.onTransitionEnd=function(e){var i=t.support.transition;return this.each(function(){if(t.isFunction(e)){var n=t(this).redraw();i?n.one(i.end,e):e()}})},t.support.touchEvents=function(){return"ontouchstart"in e||e.DocumentTouch&&i instanceof e.DocumentTouch}(),t.support.pointerEvents=function(){return e.PointerEvent||e.MSPointerEvent}(),function(){var e=t.support.touchEvents,i=t.support.pointerEvents,n=["pointerdown","MSPointerDown"],o=["pointermove","MSPointerMove"],s=["pointerup","pointerout","pointercancel","pointerleave","MSPointerUp","MSPointerOut","MSPointerCancel","MSPointerLeave"],a="touchstart",r="touchmove",d=["touchend","touchleave","touchcancel"],l="mousedown",h="mousemove",c=["mouseup","mouseleave"],u=function(t){var u,p,f;return e?(u=a+t,p=r+t,f=d.join(t+" ")+t):i?(u=n.join(t+" ")+t,p=o.join(t+" ")+t,f=s.join(t+" ")+t):(u=l+t,p=h+t,f=c.join(t+" ")+t),{start:u,move:p,end:f}},p=function(e,n){var o=n.namespace?"."+n.namespace:"",s="swipestart",a="swipemove",r="swipeend",d=u(o),l=n.data&&n.data.touchAction||"none",h=n.data&&n.data.sensitivity||5;return i&&e.css({"-ms-touch-action":""+l,"touch-action":""+l}),e.each(function(){var e=t(this),i={},n={},o=function(o){var s,r="mousemove"===o.type,d="touchmove"!==o.type&&!r,c=o.originalEvent;if(!(r&&1!==o.which||c.touches&&c.touches.length>1||o.scale&&1!==o.scale)){var u,p=(r?c.pageX:d?c.clientX:c.touches[0].pageX)-i.x,f=(r?c.pageY:d?c.clientY:c.touches[0].pageY)-i.y,m=Math.abs(parseFloat(p/e.width()*100))||100,v=Math.abs(parseFloat(f/e.height()*100))||100;switch(l){case"pan-x":Math.abs(f)>Math.abs(p)&&o.preventDefault(),u=Math.abs(f)>Math.abs(p)&&Math.abs(f)>h&&100>v;break;case"pan-y":Math.abs(p)>Math.abs(f)&&o.preventDefault(),u=Math.abs(p)>Math.abs(f)&&Math.abs(p)>h&&100>m;break;default:o.preventDefault(),u=Math.abs(f)>h||Math.abs(p)>h&&100>m&&100>v}o.stopPropagation(),u&&(s=t.Event(a,{delta:{x:p,y:f}}),e.trigger(s),s.isDefaultPrevented()||(n={x:p,y:f}))}},c=function(){var o,s=+new Date-i.time;if(Math.abs(n.x)>1||Math.abs(n.y)>1){var a=n.x<0?"left":"right",l=n.y<0?"up":"down",h=Math.abs(n.x)>Math.abs(n.y)?a:l;o=t.Event(r,{delta:n,direction:h,duration:s}),e.trigger(o)}e.off(d.move).off(d.end)};e.off(d.start).on(d.start,function(a){var r,l="mousedown"===a.type,h="touchstart"!==a.type&&!l,u=a.originalEvent;(h||l)&&t(a.target).is("img")&&a.preventDefault(),a.stopPropagation(),i={x:l?u.pageX:h?u.clientX:u.touches[0].pageX,y:l?u.pageY:h?u.clientY:u.touches[0].pageY,time:+new Date},r=t.Event(s,{start:i}),e.trigger(r),r.isDefaultPrevented()||(n={x:0,y:0},e.on(d.move,o).on(d.end,c))})})},f=function(e,i){var n=i.namespace?"."+i.namespace:"",o=u(n);return e.each(function(){t(this).css({"-ms-touch-action":"","touch-action":""}).off(o.start).off(o.move).off(o.end)})};t.event.special.swipe={add:function(e){p(t(this),e)},remove:function(e){f(t(this),e)}}}(),t.extend(t.expr[":"],{attrStart:function(e,i,n){var o=!1;return t.each(e.attributes,function(){return 0===this.name.indexOf(n[3])?(o=!0,!1):!0}),o}}),t.buildDataOptions=function(e,i,n,o){return t.each(e.data(),function(t,o){if(0===t.indexOf(n)&&t.length>n.length){var s=n.length,a=t.charAt(s).toLowerCase()+t.substring(s+1);i[a]=o,e.removeData(t)}}),o?e.data(o+"."+n+"Options",i):e.data(n+"Options",i),i},t.debounce=function(t,i,n){var o;return function(){var s=this,a=arguments;e.clearTimeout(o),o=e.setTimeout(function(){o=null,n||t.apply(s,a)},i),n&&!o&&t.apply(s,a)}},function(e){var n=t.Event("domchanged"),o=t(i);t.fn.html=function(){var t=e.apply(this,arguments);return arguments.length&&o.trigger(n),t}}(t.fn.html)}(jQuery,window,document),function(t,e,i){"use strict";if(!e.RESPONSIVE_AUTOSIZE){var n="ready"+i,o=["domchanged"+i,"shown.r.modal"].join(" "),s="resize orientationchange",a="keyup",r="paste",d="cut",l="size"+i,h="sized"+i,c=function(i,n){this.$element=t(i),this.defaults={removeAttributes:null,removeClasses:null},this.options=t.extend({},this.defaults,n),this.$clone=null,this.sizing=null,this.clone(),this.$element.on([a,r,d].join(" "),t.proxy(this.change,this)),t(e).off(s).on(s,t.debounce(t.proxy(this.size,this),50))};c.prototype.clone=function(){var e=this,i=this.options.removeAttributes,n=this.options.removeClasses,o=this.$element,s=function(){e.$clone=e.$element.clone().attr({tabindex:-1,rows:2,"aria-hidden":!0}).removeAttr("id name data-autosize "+i).removeClass(n).removeClass(n).addClass("autosize-clone").insertAfter(o),n&&e.$clone.removeData(n)};t.when(s()).then(this.size())},c.prototype.size=function(){var e,i,n=this,o=this.$element,s=this.$element[0],a=this.$clone,r=a[0],d=0,c=t.Event(l),u=function(){n.sizing=!1,o.trigger(t.Event(h))};for(a.width(o.width()),a.val(o.val()),e=a.height(),o.height(e);r.rows>1&&r.scrollHeight<r.offsetHeight;)r.rows-=1;for(;r.scrollHeight>r.offsetHeight&&d!==r.offsetHeight;)d=s.offsetHeight,r.rows+=1;if(r.rows+=1,i=a.height(),e!==i){if(o.trigger(t.Event(l)),this.sizing||c.isDefaultPrevented())return;this.sizing=!0,o.height(a.height()),o.onTransitionEnd(u)}},c.prototype.change=function(t){var i=this,n=0;("paste"===t.type||"cut"===t.type)&&(n=5),e.setTimeout(function(){i.size()},n)},t.fn.autoSize=function(e){return this.each(function(){var i=t(this),n=i.data("r.autosize"),o="object"==typeof e?e:null;n||i.data("r.autosize",n=new c(this,o)),"size"===e&&n.size()})},t.fn.autoSize.Constructor=c;var u=t.fn.autoSize;t.fn.autoSize.noConflict=function(){return t.fn.autoSize=u,this};var p=function(){t("textarea[data-autosize]").each(function(){var e=t(this),i=e.data("r.autosizeOptions");i||e.addClass("autosize").autoSize(t.buildDataOptions(e,{},"autosize","r"))})},f=t.debounce(p,500);t(document).on([n,o].join(" "),function(t){"ready"===t.type?p():f()}),e.RESPONSIVE_AUTOSIZE=!0}}(jQuery,window,".r.autosize"),function(t,e,i){"use strict";if(!e.RESPONSIVE_CAROUSEL){var n=t.support.transition,o=t.support.rtl,s="mouseenter",a="mouseleave",r="keydown",d="click",l="ready"+i,h=["domchanged"+i,"shown.r.modal"].join(" "),c="slide"+i,u="slid"+i,p={SPACE:32,LEFT:37,RIGHT:39},f=function(i,n){this.$element=t(i),this.defaults={interval:0,mode:"slide",pause:"hover",wrap:!0,keyboard:!0,touch:!0,lazyImages:!0,lazyOnDemand:!0,nextTrigger:null,nextHint:"Next ("+(o?"Left":"Right")+" Arrow)",previousTrigger:null,previousHint:"Previous ("+(o?"Right":"Left")+" Arrow)",indicators:null},this.options=t.extend({},this.defaults,n),this.paused=null,this.interval=null,this.sliding=null,this.$items=null,this.translationDuration=null,this.$nextTrigger=this.options.nextTrigger?t(this.options.nextTrigger):this.$element.find(".carousel-control.forward"),this.$previousTrigger=this.options.previousTrigger?t(this.options.previousTrigger):this.$element.find(".carousel-control.back"),this.$indicators=this.options.indicators?t(this.options.indicators):this.$element.find("ol > li"),this.id=this.$element.attr("id")||"carousel-"+t.pseudoUnique();var l=this;"fade"===this.options.mode&&this.$element.addClass("carousel-fade"),this.options.lazyImages&&!this.options.lazyOnDemand&&t(e).on("load",t.proxy(this.lazyimages),this),this.$element.attr({role:"listbox",id:this.id}),this.$element.children("figure").each(function(){var e=t(this),i=e.hasClass("carousel-active");e.attr({role:"option","aria-selected":i,tabindex:i?0:-1})});var h=this.$nextTrigger.add(this.$previousTrigger);h.each(function(){var e=t(this).attr({tabindex:0,"aria-controls":l.id});e.attr(e.is("button")?{type:"button"}:{role:"button"}),e.find(".visuallyhidden").length||t("<span/>").addClass("visuallyhidden").html(e.is(l.$nextTrigger.selector)?l.options.nextHint:l.options.previousHint).appendTo(e)}),this.$indicators.attr({role:"button","aria-controls":l.id}),"hover"===this.options.pause&&(t.support.touchEvents||t.support.pointerEvents||this.$element.on(s,t.proxy(this.pause,this)).on(a,t.proxy(this.cycle,this))),this.options.touch&&this.$element.on("swipe.carousel",{touchAction:"pan-y"},!0).on("swipemove.carousel",t.proxy(this.swipemove,this)).on("swipeend.carousel",t.proxy(this.swipeend,this)),this.options.keyboard&&this.$element.on(r,t.proxy(this.keydown,this)),t(document).on(d,"[aria-controls="+this.id+"]",t.proxy(this.click,this))};f.prototype.activeindex=function(){var t=this.$element.find(".carousel-active");return this.$items=t.parent().children("figure"),this.$items.index(t)},f.prototype.cycle=function(i){return i||(this.paused=!1),this.interval&&e.clearInterval(this.interval),this.options.interval&&!this.paused&&(this.interval=e.setInterval(t.proxy(this.next,this),this.options.interval)),this},f.prototype.to=function(e){var i=this.activeindex(),n=this;return e>this.$items.length-1||0>e?!1:this.sliding?this.$element.one(u,function(){n.to(e)}):i===e?this.pause().cycle():this.slide(e>i?"next":"prev",t(this.$items[e]))},f.prototype.pause=function(i){return i||(this.paused=!0),this.$element.find(".next, .prev").length&&t.support.transition&&(this.$element.trigger(t.support.transition.end),this.cycle(!0)),this.interval=e.clearInterval(this.interval),this},f.prototype.next=function(){return this.sliding?!1:this.slide("next")},f.prototype.prev=function(){return this.sliding?!1:this.slide("prev")},f.prototype.slide=function(e,i){var n,o,s=this.$element.children("figure.carousel-active"),a=i||s[e]("figure"),r=this.interval,d="next"===e,l=d?"left":"right",h=d?"first":"last",p=this;if(r&&this.pause(),!a.length){if(!this.options.wrap)return!1;a=this.$element.children("figure")[h]()}if(a.hasClass("carousel-active"))return this.sliding=!1;if(n=t.Event(c,{relatedTarget:a[0],direction:l}),this.$element.trigger(n),n.isDefaultPrevented())return!1;this.options.lazyImages&&this.options.lazyOnDemand&&this.lazyimages.call(a),this.sliding=!0,r&&this.pause(),this.$element.one(u,function(){p.$indicators.removeClass("active").eq(p.activeindex()).addClass("active")});var f=function(){p.$items&&p.$items.removeClass("swiping").css({"transition-duration":""}),s.removeClass(["carousel-active",l].join(" ")).attr({"aria-selected":!1,tabIndex:-1}),a.removeClass([e,l].join(" ")).addClass("carousel-active").attr({"aria-selected":!0,tabIndex:0}),p.sliding=!1,o=t.Event(u,{relatedTarget:a[0],direction:l}),p.$element.trigger(o)};return a.addClass(e).redraw(),s.addClass(l),a.addClass(l),this.$items&&this.$items.each(function(){t(this).removeClass("swipe swipe-next").css({left:"",right:"",opacity:""})}),s.onTransitionEnd(f).ensureTransitionEnd(),r&&this.cycle(),this},f.prototype.keydown=function(t){var e=t&&t.which;if(e===p.LEFT||e===p.RIGHT)switch(t.preventDefault(),t.stopPropagation(),e){case p.LEFT:o?(this.next(),this.$nextTrigger.focus()):(this.prev(),this.$previousTrigger.focus());break;case p.RIGHT:o?(this.prev(),this.$previousTrigger.focus()):(this.next(),this.$nextTrigger.focus())}},f.prototype.click=function(e){if(e){e.preventDefault(),e.stopPropagation();var i=t(e.target),n=i.is(this.$indicators.selector);n?this.to(i.index()):i.is(this.$nextTrigger.selector)?this.next():i.is(this.$previousTrigger.selector)&&this.prev()}},f.prototype.swipemove=function(t){if(!this.sliding){this.pause();var e=t.delta.x<0,i=e?o?"prev":"next":o?"next":"prev",n=e?o?"last":"first":o?"first":"last",s=this.activeindex(),a=this.$items.eq(s),r=a[i]("figure");if(1!==this.$items.length){if(!r.length){if(!this.options.wrap)return;r=this.$element.children("figure")[n]()}if(this.$items.not(a).not(r).removeClass("swipe swiping swipe-next").css({left:"",right:"",opacity:""}),!r.hasClass("carousel-active")){this.options.lazyImages&&this.options.lazyOnDemand&&this.lazyimages.call(r);var d=a.width(),l=parseFloat(t.delta.x/d*100),h=e?100:-100;o&&(l*=-1),this.$element.addClass("no-transition"),"slide"===this.options.mode?o?(a.addClass("swiping").css({right:l+"%"}),r.addClass("swipe swipe-next").css({right:l-h+"%"})):(a.addClass("swiping").css({left:l+"%"}),r.addClass("swipe swipe-next").css({left:l+h+"%"})):(a.addClass("swipe").css({opacity:1-Math.abs(l/100)}),r.addClass("swipe swipe-next"))}}}},f.prototype.swipeend=function(e){if(!this.sliding&&this.$element.hasClass("no-transition")){var i=e.direction,o="next";if("right"===i&&(o="prev"),this.$element.removeClass("no-transition"),n){var s=this.activeindex(),a=this.$items.eq(s);this.translationDuration||(this.translationDuration=parseFloat(a.css("transition-duration")));var r=a.width(),d=Math.abs(e.delta.x)/r*100,l=e.duration/1e3*100/d,h=(100-d)/100*Math.min(this.translationDuration,l);this.$items.each(function(){t(this).css({"transition-duration":h+"s"})})}this.cycle(),this.slide(o,t(this.$items.filter(".swipe-next")))}},f.prototype.lazyimages=function(){this.data("lazyLoaded")||(this.find("img[data-src]").each(function(){0===this.src.length&&(this.src=this.getAttribute("data-src"))}),this.data("lazyLoaded",!0))},t.fn.carousel=function(e){return this.each(function(){var i=t(this),n=i.data("r.carousel"),o="object"==typeof e?e:null;n||i.data("r.carousel",n=new f(this,o)),"number"==typeof e?n.to(e):"string"==typeof e&&/(cycle|pause|next|prev)/.test(e)||(e=o.slide)?n[e]():n.options.interval&&n.pause().cycle()})},t.fn.carousel.Constructor=f;var m=t.fn.carousel;t.fn.carousel.noConflict=function(){return t.fn.carousel=m,this};var v=function(){t(".carousel").each(function(){var e=t(this),i=e.data("r.carouselOptions");i||e.carousel(t.buildDataOptions(e,{},"carousel","r"))})},g=t.debounce(v,500);t(document).on([l,h].join(" "),function(t){"ready"===t.type?v():g()}),e.RESPONSIVE_CAROUSEL=!0}}(jQuery,window,".r.carousel"),function(t,e,i){"use strict";if(!e.RESPONSIVE_CONDITIONAL){var n="ready"+i,o=["domchanged"+i,"shown.r.modal"].join(" "),s=["resize","orientationchange"].join(".conditional "),a="loaded"+i,r="error"+i,d=function(i,n){this.$element=t(i),this.defaults={xs:null,s:null,m:null,l:null,fallback:null,errorHint:"<p>An error has occured.</p>"},this.cache={},this.options=t.extend({},this.defaults,n),this.currentGrid=null,this.currentTarget=null,this.sizing=null,t(e).on(s,t.debounce(t.proxy(this.resize,this),50)),this.resize()};d.prototype.resize=function(){var e=t.support.currentGrid(),i=e.grid,n=e.range;if(!this.options.fallback)for(var o in n)if(n.hasOwnProperty(o)){var s=n[o];this.options[s]||(this.options[s]="fallback",this.cache[s]=this.$element.html())}if(this.currentGrid!==i){this.currentGrid=i;var d=this,l=this.options[i]||this.options.fallback;l&&l!==this.currentTarget&&(this.currentTarget=l,this.cache[this.currentGrid]?(this.$element.empty().html(this.cache[this.currentGrid]),this.$element.trigger(t.Event(a,{relatedTarget:d.$element[0],loadTarget:l,grid:this.currentGrid}))):this.$element.empty().load(l,null,function(e,n){if("error"===n)return d.$element.trigger(t.Event(r,{relatedTarget:d.$element[0],loadTarget:l,grid:d.currentGrid})),void d.$element.html(d.options.errorHint);var o,s=l.indexOf(" ");s>=0&&(o=t.trim(l.slice(s))),d.cache[i]=o?jQuery("<div>").append(t.parseHTML(e)).find(o).wrap("<div>").parent().html():e,d.$element.trigger(t.Event(a,{relatedTarget:d.$element[0],loadTarget:l,grid:d.currentGrid}))}))}},t.fn.conditional=function(e){return this.each(function(){var i=t(this),n=i.data("r.conditional"),o="object"==typeof e?e:null;n||i.data("r.conditional",n=new d(this,o)),"resize"===e&&n.resize()})},t.fn.conditional.Constructor=d;var l=t.fn.conditional;t.fn.conditional.noConflict=function(){return t.fn.conditional=l,this};var h=function(){t(":attrStart(data-conditional)").each(function(){var e=t(this),i=e.data("r.conditionalOptions");i||e.conditional(t.buildDataOptions(e,{},"conditional","r"))})},c=t.debounce(h,500);t(document).on([n,o].join(" "),function(t){"ready"===t.type?h():c()}),e.RESPONSIVE_CONDITIONAL=!0}}(jQuery,window,".r.conditional"),function(t,e,i){"use strict";if(!e.RESPONSIVE_DISMISS){var n="ready"+i,o=["domchanged"+i,"shown.r.modal"].join(" "),s="click",a="dismiss"+i,r="dismissed"+i,d=function(e,i){this.defaults={closeHint:"Click to close"},this.options=t.extend({},this.defaults,i),this.$element=t(e).attr({type:"button"}),this.$target=this.$element.closest(i.target),this.dismissing=null,this.$element.is("button")&&t(e).attr({type:"button"}),this.$target.hasClass("alert")&&this.$target.attr({role:"alert"}),this.$element.find(".visuallyhidden").length||t("<span/>").addClass("visuallyhidden").html(this.options.closeHint).appendTo(this.$element),this.$element.on(s,t.proxy(this.click,this))};d.prototype.close=function(){var e=t.Event(a),i=this.$target,n=this,o=function(){n.dismissing=!1,i.addClass("hidden").attr({"aria-hidden":!0,tabindex:-1}),n.$element.trigger(t.Event(r))};this.$element.trigger(e),this.dismissing||e.isDefaultPrevented()||(this.dismissing=!0,i.addClass("fade-in fade-out").redraw().removeClass("fade-in"),this.$target.onTransitionEnd(o))},d.prototype.click=function(t){t.preventDefault(),this.close()},t.fn.dismiss=function(e){return this.each(function(){var i=t(this),n=i.data("dismiss");n||i.data("dismiss",n=new d(this,e)),"close"===e&&n.close()})},t.fn.dismiss.Constructor=d;var l=t.fn.dismiss;t.fn.dismiss.noConflict=function(){return t.fn.dismiss=l,this};var h=function(){t("button[data-dismiss-target]").each(function(){var e=t(this),i=e.data("r.dismissOptions");i||e.dismiss(t.buildDataOptions(e,{},"dismiss","r"))})},c=t.debounce(h,500);t(document).on([n,o].join(" "),function(t){"ready"===t.type?h():c()}),e.RESPONSIVE_DISMISS=!0}}(jQuery,window,".r.dismiss"),function(t,e,i){"use strict";if(!e.RESPONSIVE_DROPDOWN){var n=e.getComputedStyle&&t.support.transition,o=t.support.rtl,s="ready"+i,a=["domchanged"+i,"shown.r.modal"].join(" "),r="click",d="keydown",l="show"+i,h="shown"+i,c="hide"+i,u="hidden"+i,p={SPACE:32,LEFT:37,RIGHT:39},f=function(e,i){this.$element=t(e),this.$target=t(i.target),this.defaults={dimension:"height"},this.options=t.extend({},this.defaults,i),this.$parent=null,this.transitioning=null,this.endSize=null,this.options.parent&&(this.$parent=this.$target.closest(this.options.parent)),this.$parent?this.$parent.attr({role:"tablist","aria-multiselectable":"true"}).find("div:not(.collapse,.accordion-body)").attr("role","presentation"):t(".accordion").find("div:not(.collapse,.accordion-body)").addBack().attr("role","presentation");var n=t("[href='"+this.options.target+"'], [data-dropdown-target='"+this.options.target+"']"),o=n.attr("id")||"dropdown-"+t.pseudoUnique(),s=this.$target.attr("id")||"dropdown-"+t.pseudoUnique(),a=!this.$target.hasClass("collapse");n.attr({id:o,role:"tab","aria-controls":s,"aria-selected":a,"aria-expanded":a,tabindex:0}),this.$target.attr({id:s,role:"tabpanel","aria-labelledby":o,"aria-hidden":!a,tabindex:a?0:-1}),this.$element.on(r,t.proxy(this.click,this)),this.$element.on(d,t.proxy(this.keydown,this))};f.prototype.show=function(){if(!this.transitioning&&!this.$target.hasClass("expand")){var i=this,o=this.options.dimension,s=[];this.$parent&&(s=this.$parent.find(" > [role=presentation] > [role=presentation]").children("[role=tab]"),s=t.grep(s,function(e){var n=t(e).data("r.dropdown"),o=n&&n.$target;return o&&o.hasClass("dropdown-group")&&!o.hasClass("collapse")&&n.$parent&&n.$parent[0]===i.$parent[0]})),this.$target[o](0),n&&(this.$target[o]("auto"),this.endSize=e.getComputedStyle(this.$target[0])[o],this.$target[o](0).redraw()),this.$target[o](this.endSize||""),this.transition("removeClass",t.Event(l),h),s&&s.length&&t.each(s,function(){t(this).dropdown("hide")})}},f.prototype.hide=function(){if(!this.transitioning&&!this.$target.hasClass("collapse")){var i,o=this.options.dimension;n&&(i=e.getComputedStyle(this.$target[0])[o],this.$target[o](i).redraw()),this.$target.removeClass("expand"),this.$target[o](0),this.transition("addClass",t.Event(c),u)}},f.prototype.toggle=function(){this[this.$target.hasClass("collapse")?"show":"hide"]()},f.prototype.transition=function(e,i,n){var o=this,s="removeClass"===e,a=function(){var e=t.Event(n);o.$target.removeClass("trans")[o.options.dimension](""),o.transitioning=!1,o.$target.attr({"aria-hidden":!s,tabindex:s?0:-1});var i=t("#"+o.$target.attr("aria-labelledby")).attr({"aria-selected":s,"aria-expanded":s});s&&i.focus(),o.$target.find("[tabindex]:not(.collapse)").attr({"aria-hidden":!s,tabindex:s?0:-1}),o.$element.trigger(e)};this.transitioning||i.isDefaultPrevented()||(this.transitioning=!0,this.$element.trigger(i),this.$target[e]("collapse"),this.$target["show"===i.type?"addClass":"removeClass"]("expand trans"),this.$target.onTransitionEnd(a))},f.prototype.click=function(t){t.preventDefault(),t.stopPropagation(),this.toggle()},f.prototype.keydown=function(e){var i=e.which;if(i===p.SPACE||i===p.LEFT||i===p.RIGHT){e.preventDefault(),e.stopPropagation();var n=t(e.target),s=n.closest(this.options.parent?"[role=tablist]":".accordion"),a=s.find(" > [role=presentation] > [role=presentation]").children("[role=tab]"),r=a.index(a.filter(":focus")),d=a.length;if(i===p.SPACE)return void t(a.eq(r)).data("r.dropdown").toggle();i===p.LEFT?o?r+=1:r-=1:i===p.RIGHT&&(o?r-=1:r+=1),r===d&&(r=0),0>r&&(r=d-1),t(a.eq(r)).data("r.dropdown").show()}},t.fn.dropdown=function(e){return this.each(function(){var i=t(this),n=i.data("r.dropdown"),o="object"==typeof e?e:null;n||i.data("r.dropdown",n=new f(this,o)),"string"==typeof e&&/(show|hide|toggle)/.test(e)&&n[e]()})},t.fn.dropdown.Constructor=f;var m=t.fn.dropdown;t.fn.dropdown.noConflict=function(){return t.fn.dropdown=m,this};var v=function(){t(":attrStart(data-dropdown)").each(function(){var e=t(this),i=e.data("r.dropdownOptions");i||e.dropdown(t.buildDataOptions(e,{},"dropdown","r"))})},g=t.debounce(v,500);t(document).on([s,a].join(" "),function(t){"ready"===t.type?v():g()}),e.RESPONSIVE_DROPDOWN=!0}}(jQuery,window,".r.dropdown"),function(t,e,i){"use strict";if(!e.RESPONSIVE_MODAL){var n=t(e),o=t("html"),s=t("body"),a=t("<div/>").attr({role:"document"}).addClass("modal-overlay modal-loader fade-out"),r=t("<div/>").addClass("modal fade-out").appendTo(a),d=t("<div/>").addClass("modal-header fade-out"),l=t("<div/>").addClass("modal-footer fade-out"),h=t("<button/>").attr({type:"button"}).addClass("modal-close fade-out"),c=t("<button/>").attr({type:"button"}).addClass("modal-direction prev fade-out"),u=t("<button/>").attr({type:"button"}).addClass("modal-direction next fade-out"),p=t("<div/>").addClass("modal-placeholder"),f="ready"+i,m="domchanged"+i,v=["resize","orientationchange"].join(".modal "),g="click",y="keydown",b="focusin",w="show"+i,$="shown"+i,x="hide"+i,C="hidden"+i,E="error"+i,T=t.support.rtl,S=t.support.transition,P=t.support.currentGrid(),O={ESCAPE:27,LEFT:37,RIGHT:39},D=0,z=0===e.location.protocol.indexOf("http")?e.location.protocol:"http:",k=new RegExp("//"+e.location.host+"($|/)"),I=/(^data:image\/.*,)|(\.(jp(e|g|eg)|gif|png|bmp|ti(ff|f)|webp|svg)((\?|#).*)?$)/i,j=/^#.*$/,A=/^([\w.+-]+:)(?:\/\/([^\/?#:]*)(?::(\d+)|)|)/,M=/^(?:about|app|app-storage|.+-extension|file|res|widget):$/,H=function(i,n){this.$element=t(i),this.defaults={modal:null,external:!1,group:null,iframe:!1,iframeScroll:!0,keyboard:!0,touch:!0,next:">",nextHint:"Next ("+(T?"Left":"Right")+" Arrow)",prev:"<",previousHint:"Previous ("+(T?"Right":"Left")+" Arrow)",closeHint:"Close (Esc)",errorHint:"<p>An error has occured.</p>",mobileTarget:null,mobileViewportWidth:"xs",fitViewport:!0},this.options=t.extend({},this.defaults,n),this.title=null,this.description=null,this.isShown=null,this.$group=null,this.options.group&&(this.$group=t(this.options.group)),this.$element.on(g,t.proxy(this.click,this));var o=t.debounce(t.proxy(this.resize,this),15);t(e).off(v).on(v,o)};H.prototype.show=function(){if(!this.isShown){if(this.options.mobileTarget){var i=this.options.mobileViewportWidth;if("number"==typeof i&&i>=n.width())return void(e.location.href=this.options.mobileTarget);if("string"==typeof i){var o=t.inArray(i,P.range);if(P.index<=o&&o>-1)return void(e.location.href=this.options.mobileTarget)}}var s=this,d=t.Event(w),l=t.Event($),p=function(){r.data("currentModal",s.$element),r.focus(),t(document).on(b,function(e){if(e.target!==a[0]&&!t.contains(a[0],e.target)){var i=r.find("input, select, a, iframe, img, button").first();return i.length?i.focus():!s.options.modal&&h.focus()||a.focus(),!1}return!0}),s.options.keyboard&&t(document).on(y,t.proxy(s.keydown,s)),s.options.group&&s.options.touch&&r.on("swipe.modal",!0).on("swipeend.modal",t.proxy(s.swipeend,s)),r.off(g).on(g,t.proxy(function(t){var e=u[0],i=c[0],n=t.target;return n===e||n===i?(t.preventDefault(),t.stopPropagation(),void this[n===e?"next":"prev"]()):void(this.options.modal&&n===r.find(this.options.modal)[0]&&(t.preventDefault(),t.stopPropagation(),this.hide()))},s)),s.$element.trigger(l)};this.$element.trigger(d),d.isDefaultPrevented()||(this.isShown=!0,this.overlay(),this.create(),r.onTransitionEnd(p))}},H.prototype.hide=function(e,i){if(this.isShown){var n=this,o=t.Event(x),s=t.Event(C),a=function(){r.removeData("currentModal"),n.$element.trigger(s)};this.$element.trigger(o),o.isDefaultPrevented()||(this.isShown=!1,this.destroy(i),e||this.overlay(!0),r.onTransitionEnd(a))}},H.prototype.overlay=function(e){var i=e?"removeClass":"addClass",d=this,l=function(){return e?(a.addClass("hidden"),o.removeClass("modal-on").css("margin-right",""),void(o.hasClass("modal-lock")&&(o.removeClass("modal-lock"),D!==n.scrollTop()&&(n.scrollTop(D),D=0)))):void a.off(g).on(g,function(e){if(!d.options.modal){var i=h[0],n=e.target;n===r[0]||t.contains(r[0],n)||(n===i&&(e.preventDefault(),e.stopPropagation(),d.hide()),(n===a[0]||t.contains(a[0],n))&&d.hide())}})},c=function(){var e=t("<div/>").css({width:99,height:99,overflow:"scroll",position:"absolute",top:-9999});s.append(e);var i=e[0].offsetWidth-e[0].clientWidth;return e.remove(),i};t(".modal-overlay").length||s.append(a),e||(0===D&&(D=n.scrollTop()),o.addClass("modal-on").css("margin-right",c())),a.removeClass("hidden").redraw()[i]("fade-in").redraw(),a.onTransitionEnd(l)},H.prototype.create=function(){a.addClass("modal-loader"),this.options.external=!j.test(this.options.target);var e=function(t){var e=A.exec(t)||A.exec(z+t);return void 0===e||j.test(t)?!1:e&&e[2]&&!M.test(e[1])?!k.test(e[2]):!1},i=function(){n.resize(),t.each([d,l,h,u,c,r],function(){this.addClass("fade-in").redraw()}),a.removeClass("modal-loader")},n=this,o=this.options.title,s=this.options.description,f=this.options.modal,m=this.options.target,v=e(m),g=!this.options.external&&!v,y=this.$group,b=this.options.next+'<span class="visuallyhidden">'+this.options.nextHint+"</span>",w=this.options.prev+'<span class="visuallyhidden">'+this.options.prevHint+"</span>",$=this.options.iframeScroll,x=this.options.iframe||!g?v&&!I.test(m):!1,C=t("<div/>").addClass($?"media media-scroll":"media"),T=t("<div/>").addClass("modal-content");if(y){var S=y.filter(function(){return t(this).data("r.modal")});S.length&&(u.html(b).prependTo(r),c.html(w).prependTo(r))}if(o||!f){if(o){var P="modal-label-"+t.pseudoUnique();d.html('<div id="navBar"><h2 id="'+P+'">'+o+"</h2></div>").appendTo(a.attr({"aria-labelledby":P}))}f||h.html('x <span class="visuallyhidden">'+this.options.closeHint+"</span>").appendTo(a)}if(s&&l.html('<div id="navBar">'+s+"</div>").appendTo(a),g){var O=t(m);this.isLocalHidden=O.is(":hidden"),r.addClass(this.options.fitViewport?"container":""),p.detach().insertAfter(O),O.detach().appendTo(T).removeClass("hidden"),T.appendTo(r),i()}else if(x){r.addClass("modal-iframe");var D=0!==m.indexOf("http")?z+m:m,H=function(t){var e={youtube:/youtu(be\.com|be\.googleapis\.com|\.be)/i,vimeo:/vimeo/i,vine:/vine/i,instagram:/instagram|instagr\.am/i,getty:/embed\.gettyimages\.com/i};for(var i in e)if(e.hasOwnProperty(i)&&e[i].test(t))return[i,"scaled"].join(" ");return!1};t("<iframe/>").attr({scrolling:$?"yes":"no",allowTransparency:!0,frameborder:0,hspace:0,vspace:0,webkitallowfullscreen:"",mozallowfullscreen:"",allowfullscreen:""}).one("load error",function(){i()}).appendTo(C).attr("src",D);var R=H(m)||"";C.addClass(R).appendTo(r)}else I.test(m)?(r.addClass("modal-image"),t("<img/>").one("load error",function(){i()}).appendTo(r).attr("src",m)):(r.addClass("modal-ajax"),r.addClass(this.options.fitViewport?"container":""),T.load(m,null,function(e,o){"error"===o&&(n.$element.trigger(t.Event(E,{relatedTarget:T[0]})),T.html(n.options.errorHint)),T.appendTo(r),i()}))},H.prototype.destroy=function(i){var n=this;t.each([d,l,h,r,u,c],function(){this.removeClass("fade-in").redraw()}),r.onTransitionEnd(function(){u.detach(),c.detach(),d.empty().detach(),l.empty().detach(),h.detach(),a.removeAttr("aria-labelledby"),n.options.external||(t(n.options.target).addClass(n.isLocalHidden?"hidden":"").detach().insertAfter(p),p.detach().insertAfter(a)),t.when(r.find("iframe").attr("src","")).then(e.setTimeout(function(){r.removeClass("modal-iframe modal-ajax modal-image container").css({"max-height":"","max-width":""}).empty(),t(document).off(b),n.options.keyboard&&t(document).off(y),n.options.touch&&r.off("swipe.modal swipeend.modal"),i&&i.call(n)},100))})},H.prototype.click=function(t){t.preventDefault();var i=r.data("currentModal");if(i&&i[0]!==this.$element[0]){var n=this,o=function(){S?n.show():e.setTimeout(function(){n.show()},300)};return void i.data("r.modal").hide(!0,o)}this.show()},H.prototype.keydown=function(t){this.options.modal||(t.which===O.ESCAPE&&this.hide(),this.options.group&&(t.which===O.LEFT&&(T?this.next():this.prev()),t.which===O.RIGHT&&(T?this.prev():this.next())))},H.prototype.resize=function(){var i=n.height(),s=d.length&&d.height()||0,a=h.length&&h.outerHeight()||0,c=a>s?a:s,u=l.length&&l.height()||0,p=.95*(i-(c+u));if(t(".modal-overlay").css({"padding-top":c,"padding-bottom":u}),r.hasClass("modal-image"))r.children("img").css("max-height",p);else if(r.hasClass("modal-iframe")){var f=r.find(".media > iframe"),m=f.width(),v=f.height(),g=m/v,y=p*g;f.parent().hasClass("scaled")&&r.css({"max-height":p,"max-width":y})}else{var b=r.children(".modal-content");t.each([r,b],function(){this.css({"max-height":p})}),e.MSPointerEvent&&b.length&&b.children("*:first")[0].scrollHeight>b.height()&&o.addClass("modal-lock")}P=t.support.currentGrid()},H.prototype.direction=function(i){if(this.isShown&&this.options.group){var n=this,o=this.$group.index(this.$element),s=this.$group.length,a="next"===i?o+1:o-1,r=function(){n.$sibling&&n.$sibling.data("r.modal")&&(S?n.$sibling.data("r.modal").show():e.setTimeout(function(){n.$sibling.data("r.modal").show()},300))};"next"===i?(a>=s||0>a)&&(a=0):(a>=s&&(a=0),0>a&&(a=s-1)),this.$sibling=t(this.$group[a]),this.hide(!0,r)}},H.prototype.next=function(){this.direction("next")},H.prototype.prev=function(){this.direction("prev")},H.prototype.swipeend=function(t){return T?void this["right"===t.direction?"prev":"next"]():void this["right"===t.direction?"next":"prev"]()},t.fn.modal=function(e){return this.each(function(){var i=t(this),n=i.data("r.modal"),o="object"==typeof e?e:{};o.target||(o.target=i.attr("href")),n||i.data("r.modal",n=new H(this,o)),"string"==typeof e&&/(show|hide|next|prev)/.test(e)&&n[e]()
})},t.fn.modal.Constructor=H;var R=t.fn.modal;t.fn.modal.noConflict=function(){return t.fn.modal=R,this};var L=function(){t(":attrStart(data-modal)").each(function(){var e=t(this),i=e.data("r.modalOptions");i||e.modal(t.buildDataOptions(e,{},"modal","r"))})},N=t.debounce(L,500);t(document).on([f,m,$].join(" "),function(t){"ready"===t.type?L():N()}),e.RESPONSIVE_MODAL=!0}}(jQuery,window,".r.modal"),function(t,e,i){"use strict";if(!e.RESPONSIVE_TABLE){var n="ready"+i,o=["domchanged"+i,"shown.r.modal"].join(" "),s="add"+i,a="added"+i,r=function(e){this.$element=t(e).addClass("table-list"),this.$thead=this.$element.find("thead"),this.$tfoot=this.$element.find("tfoot"),this.$tbody=this.$element.find("tbody"),this.$headerColumns=this.$thead.find("th"),this.$footerColumns=this.$tfoot.find("th"),this.$bodyRows=this.$tbody.find("tr"),this.isAdded=null,this.add()};r.prototype.add=function(){if(!this.isAdded){var e=this,i=t.Event(s),n=function(){e.$element.trigger(t.Event(a))};this.$element.trigger(i),i.isDefaultPrevented()||(this.isAdded=!0,t.each(this.$bodyRows,function(){t(this).find("th, td").each(function(i){var n=t(this),o=t(e.$headerColumns[i]).text();if(n.attr("data-thead",o),e.$tfoot.length){var s=t(e.$footerColumns[i]).text();n.attr("data-tfoot",s)}})}),this.$element.redraw().addClass("fade-in"),this.$element.onTransitionEnd(n))}},t.fn.tablelist=function(e){return this.each(function(){var i=t(this),n=i.data("r.tablelist"),o="object"==typeof e?e:null;n||i.data("r.tablelist",n=new r(this,o)),"string"==typeof e&&n[e]()})},t.fn.tablelist.Constructor=r;var d=t.fn.table;t.fn.tablelist.noConflict=function(){return t.fn.tablelist=d,this};var l=function(){t("table[data-table-list]").each(function(){var e=t(this),i=e.data("r.tablelistOptions");i||e.tablelist(t.buildDataOptions(e,{},"tablelist","r"))})},h=t.debounce(l,500);t(document).on([n,o].join(" "),function(t){"ready"===t.type?l():h()}),e.RESPONSIVE_TABLE=!0}}(jQuery,window,".r.tablelist"),function(t,e,i){"use strict";if(!e.RESPONSIVE_TABS){var n=t.support.rtl,o="ready"+i,s=["domchanged"+i,"shown.r.modal"].join(" "),a="click",r="keydown",d="show"+i,l="shown"+i,h={SPACE:32,LEFT:37,RIGHT:39},c=function(e){this.$element=t(e),this.tabbing=null;var i=this.$element.children("ul:first").attr("role","tablist"),n=i.children().attr("role","presentation"),o=this.$element.children(":not(ul)"),s=t.pseudoUnique();n.each(function(e){var i=t(this),n=i.children("a");n.attr({role:"tab",id:"tab-"+s+"-"+e,"aria-controls":"pane-"+s+"-"+e,"aria-selected":i.hasClass("tab-active")?!0:!1,tabIndex:0}),o.eq(e).attr({role:"tabpanel",id:"pane-"+s+"-"+e,"aria-labelledby":"tab-"+s+"-"+e,tabIndex:i.hasClass("tab-active")?0:-1})}),t(this.$element).on(a,"ul[role=tablist] > li > [role=tab]",t.proxy(this.click,this)).on(r,"ul[role=tablist] > li > [role=tab]",t.proxy(this.keydown,this))};c.prototype.show=function(e){var i=this.$element.children("ul").children(".tab-active"),n=i.parent().children(),o=n.index(i),s=this;return e>n.length-1||0>e?!1:o===e?!1:this.tab(o,e,function(e){var i=function(){s.tabbing=!1,s.$element.trigger(t.Event(l))};e.onTransitionEnd(i)})},c.prototype.tab=function(e,i,n){var o=t.Event(d),s=this.$element,a=s.children("ul").children("li"),r=s.children(":not(ul)"),l=a.eq(i),h=r.eq(e),c=r.eq(i);s.trigger(o),this.tabbing||o.isDefaultPrevented()||(this.tabbing=!0,a.removeClass("tab-active").children("a").attr({"aria-selected":!1}),l.addClass("tab-active").children("a").attr({"aria-selected":!0}).focus(),h.addClass("fade-out fade-in"),c.attr({tabIndex:0}).addClass("tab-pane-active fade-out"),r.filter(".fade-in").attr({tabIndex:-1}).removeClass("tab-pane-active fade-in"),c.redraw().addClass("fade-in"),n.call(this,c))},c.prototype.click=function(e){e.preventDefault(),e.stopPropagation();var i=t(e.target),n=i.parent(),o=n.index();this.show(o)},c.prototype.keydown=function(e){var i=e.which;if(i===h.SPACE||i===h.LEFT||i===h.RIGHT){e.preventDefault(),e.stopPropagation();var o=t(e.target),s=o.parent(),a=s.siblings().addBack(),r=a.length,d=s.index();if(i===h.SPACE)return void this.show(d);d=i===h.LEFT?n?d+1:d-1:n?d-1:d+1,d===r&&(d=0),0>d&&(d=r-1),this.show(d)}},t.fn.tabs=function(e){return this.each(function(){var i=t(this),n=i.data("r.tabs");n||i.data("r.tabs",n=new c(this)),"number"==typeof e&&n.show(e)})},t.fn.tabs.Constructor=c;var u=t.fn.tabs;t.fn.tabs.noConflict=function(){return t.fn.tabs=u,this};var p=function(){t("[data-tabs]").each(function(){var e=t(this),i=e.data("r.tabsLoaded");i||(e.data("r.tabsLoaded",!0),e.tabs())})},f=t.debounce(p,500);t(document).on([o,s].join(" "),function(t){"ready"===t.type?p():f()}),e.RESPONSIVE_TABS=!0}}(jQuery,window,".r.tabs");