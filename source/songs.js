// EcmaScript functions for Songs data

function Scale_Px(event, low_width, low_height) {
    event.currentTarget.width  = window.devicePixelRatio * low_width;
    event.currentTarget.height = window.devicePixelRatio * low_height;
}

const Return = 13;
const Escape = 27;
const Space  = 32;

function EditCategory(event) {
    var parent = event.currentTarget;
    // the <div> containing the category list

    if (("keyCode" in event && (event.keyCode == Return || event.keyCode == Space)) ||
        "button" in event) {
        // A click, or keyboard <ret> or <space>
        if (parent.classList.contains("categories_list")) {
            var el = event.currentTarget; // the <div> containing the category list

            el.innerHTML = "<form action=\"/update\" method=\"post\" class=\"category_edit\">" +
                "<input type=hidden name=ref value=\"" + encodeURI(document.baseURI) + "\">" +
                "<input type=hidden name=id readonly value=\"" + parent.getAttribute("id") +"\">" +
                "<input type=text id=\"edit_focus\" name=category value=\"" + parent.innerText + "\"" +
                " onkeydown=\"EditingKey(event)\">" +
                "<div><input type=submit value=Save>" +
                "<input id=\"edit_cancel\" type=submit formnovalidate name=cancel value=\"Cancel\"></div>" +
                "</form>";

            // Set the focus after the document is actually updated.
            window.setTimeout(function(){document.getElementById("edit_focus").focus();});
            
            parent.classList.remove("categories_list");
            parent.classList.add("categories_edit");
        }
    }
}

function EditingKey(event) {
    if (event.keyCode == Escape) {
        document.getElementById("edit_cancel").click();
    }
}

//  Apparently global variables holding current active tab, button
//  don't work in FireFox. 

function SelectTab(buttonId, tabId) {
    var i;
    var buttons = document.getElementsByClassName("tabbutton");
    for (i = 0; i < buttons.length; i++) {
        if (buttons[i].id == buttonId) {
            if (buttons[i].classList.contains("active")) {
                // already active; do nothing
            } else {
                buttons[i].style.backgroundColor = 'palegreen';
                buttons[i].classList.add("active");
            }
        } else {
            if (buttons[i].classList.contains("active")) {
                buttons[i].style.backgroundColor = 'darkseagreen';
                buttons[i].classList.remove("active");
            }
        }
    }
    
    var tabs = document.getElementsByClassName("tabcontent");
    for (i = 0; i < tabs.length; i++) {
        if (tabs[i].id == tabId) {
            if (tabs[i].classList.contains("active")) {
                // already active; do nothing
            } else {
                tabs[i].style.display = "block";
                tabs[i].classList.add("active");
            }
        } else {
            if (tabs[i].classList.contains("active")) {
                tabs[i].style.display = "none";
                tabs[i].classList.remove("active");
            }
        }
    }
}

function InitTabs() {
    SelectTab('general_search_button', 'general_search_tab');
}
// end of file
