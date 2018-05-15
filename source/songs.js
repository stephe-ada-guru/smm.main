// EcmaScript functions for Songs data

function Scale_Px(event, low_width, low_height) {
    event.currentTarget.width  = window.devicePixelRatio * low_width;
    event.currentTarget.height = window.devicePixelRatio * low_height;
}

function EditCategory(event) {
    var parent = event.currentTarget;
    // the <div> containing the category list
    
    if (parent.classList.contains("categories_list")) {
        var el = event.currentTarget; // the <div> containing the category list

        el.innerHTML = "<form action=\"/update\" method=\"post\" class=\"category_edit\">" +
            "<input type=hidden name=ref value=\"" + encodeURI(document.baseURI) + "\">" +
            "<input type=hidden name=id readonly value=\"" + parent.getAttribute("id") +"\">" +
            "<input type=text id=\"edit_focus\" name=category value=\"" + parent.innerText + "\">" +
            "<div><input type=submit value=Save>" +
            "<input type=submit formnovalidate name=cancel value=\"Cancel\"></div>" +
            "</form>";

        // Set the focus after the document is actually updated.
        window.setTimeout(function(){document.getElementById("edit_focus").focus();});
        
        parent.classList.remove("categories_list");
        parent.classList.add("categories_edit");
    }

}
// end of file
