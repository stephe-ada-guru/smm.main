// EcmaScript functions for Songs data

function Scale_Px(event, low_width, low_height) {
    event.currentTarget.width  = window.devicePixelRatio * low_width;
    event.currentTarget.height = window.devicePixelRatio * low_height;
}

// Select stylesheet according to pixel density. devicePixelRatio
// gives 'px' scaling; it isn't strong enough on Android; we
// effectively square it. And it doesn't apply to fonts in the first
// place.
if (window.devicePixelRatio < 2) {
    document.write("<link type=\"text/css\" rel=\"stylesheet\" href=\"/server_data/songs-ldpi.css\"/>");
} else if (window.devicePixelRatio < 3) {
    document.write("<link type=\"text/css\" rel=\"stylesheet\" href=\"/server_data/songs-mdpi.css\"/>");
} else {
    document.write("<link type=\"text/css\" rel=\"stylesheet\" href=\"/server_data/songs-hdpi.css\"/>");
};

// end of file
