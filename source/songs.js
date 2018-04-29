// EcmaScript functions for Songs data

function Scale_Px(event, low_width, low_height) {
    event.currentTarget.width  = window.devicePixelRatio * low_width;
    event.currentTarget.height = window.devicePixelRatio * low_height;
}

// end of file
