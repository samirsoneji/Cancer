    $(document).ready( function() {
        $('a').filter( function() {
            if (!this.hostname || $(this).find("img").length > 0) {
                return false;
                }
            var link_hostname = this.hostname;
            //chrome has a bug that returns the port in the hostname for a link, but not the location, awesome!
            if (link_hostname.indexOf(':') >= 0) {
                link_hostname = link_hostname.substring(0,link_hostname.indexOf(':'));
            }
            return !(link_hostname == location.hostname || /\.gov$/.test(link_hostname) );
        }).after(' <a class="extlink" href="/external.html"><img  src="/images/external.gif" alt="External Web Site Policy" title="External Web Site Policy"/></a> ');
    });