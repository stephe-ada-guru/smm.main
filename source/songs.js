// EcmaScript functions for Songs data

function Scale_Px(event, low_width, low_height) {
    event.currentTarget.width  = window.devicePixelRatio * low_width;
    event.currentTarget.height = window.devicePixelRatio * low_height;
}

function EditCategory(event) {
    var content, el, url;
    uri = document.baseURI;
    el = event.currentTarget; // the <div> containing the category list
    if (el.classList.contains("categories_list")) {
        el.classList.remove("categories_list");
        el.classList.add("categories_edit");
        content = el.innerText; // the category list
        el.innerHTML = "<form action=\"/update\" method=\"post\" class=\"category_edit\">" +
            "<input type=hidden name=ref value=\"" + encodeURI(uri) + "\">" +
            "<input type=hidden name=id readonly value=\"" + el.getAttribute("id") +"\">" +
            "<input type=text name=category value=\"" + content + "\">" +
            "<div><input type=submit value=Save>" +
            "<input type=submit formnovalidate name=cancel value=\"Cancel\"></div>" +
            "</form>";
    }
}
// end of file
