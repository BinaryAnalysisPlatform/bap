
function getCfgFileName(f){
    return TESTDIR + '/' + binaryName + '/sacfg/' + f + '.sacfg';
}

function mapNode(dict){
    g_tree_svg = {};
    var matches = Ext.query(".node");
    for(var i = 0; i < matches.length; i++){
        el = matches[i].getElementsByTagName("title");
        g_tree_svg[el[0].textContent] = matches[i].id;
    }
    // {text, id}
    matches = Ext.query(".cluster");
    for(var i = 0; i < matches.length; i++){
        el = matches[i].getElementsByTagName("text");
        g_tree_svg[el[0].textContent] = matches[i].id;
    }
    //console.log(g_tree_svg);
}
