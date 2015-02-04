/* utils */
function getTabHtmlName(f, tab){
    return TESTDIR + '/' + binaryName + '/' + tab + '/' + f + '.html';
}

function getBilHtmlName(f){
    return TESTDIR + '/' + binaryName + '/bil/' + f + '.html';
}

/* displaying function for dissasembly tag */
function dismTabDisplay(previousFunction, displayFunction){
    //console.log(previousFunction, displayFunction);
    var p_ele = frames['dism'].document.getElementById(previousFunction);
    if (p_ele != undefined)
        p_ele.classList.remove('highlight-code');
    var ele = frames['dism'].document.getElementById(displayFunction);
    ele.classList.add('highlight-code');
    ele.scrollIntoView();
}

/* displaying function for hi-bil and bil tabs */

function tabDisplay(functionName, tab){
    tabHtml = getTabHtmlName(functionName, tab);
    Ext.getCmp(tab).body.update('<iframe id="' + tab + '" src="' + tabHtml + '" style="padding: 0px 0px 0px 10px; width: 100%; height: 100%;" seamless></iframe>');

    //console.log(frames["hils"].document);
    //console.log(Ext.query("div", frames['hil'].document));
    //console.log(frames['hil'].document.querySelectorAll("div"));
}

function tabNodeDisplay(previousId, displayId, tab){
    console.log(previousId, displayId);
    try{
        var p_ele = frames[tab].document.getElementById(previousId);
        p_ele.classList.remove('highlight-code');
    }
    catch(err){}
    try{
        var ele = frames[tab].document.getElementById(displayId);
        ele.classList.add('highlight-code');
        ele.scrollIntoView();
    }
    catch(err){}
}

function tabsNodeDisplay(previousId, displayId){
    tabNodeDisplay(previousId, displayId, 'hil');
    tabNodeDisplay(previousId, displayId, 'bil');
    g_previousNode = displayId;
}

function resetTabs(){
    tabNodeDisplay(g_previousNode, '', 'hil');
    tabNodeDisplay(g_previousNode, '', 'bil');
    g_previousNode = '' 
}
