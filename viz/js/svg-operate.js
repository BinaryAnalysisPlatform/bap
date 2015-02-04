var selected = "rgb(56, 146, 211)";

function getGraphHeight(ele){
    polygonSelector = ele.select("polygon");
    points = polygonSelector.elements[0].getAttribute("points").split(" ");
    leftTop = points[0].split(",");
    rightBottom = points[2].split(",");
    height = globalRatio * (parseInt(leftTop[1]) - parseInt(rightBottom[1]));
    return height;
}

function markBB(Id, color){
    var ele = Ext.get(Id);
    var polygonSelector = ele.select("polygon");
    polygonSelector.set({"fill" : color});
}


// This is to centerize svg and to fill transparent place into white.
function polishSvg(){
    // centerize svg
    el = document.getElementsByTagName("svg")[0];
    el.style.display = "block";
    el.style.margin = "auto";
    // fill svg
    el = Ext.query("polygon");
    for(var i = 0; i < el.length; i++){
        if(el[i].parentNode.className.baseVal != "edge")
            el[i].setAttribute("fill", "white");
    }
    el = Ext.query("ellipse");
    for(var i = 0; i < el.length; i++)
        el[i].setAttribute("fill", "white");
    // Fill BB_Entry and BB_Exit as another color. We pick selected at this moment.
}

function resetGraph(){
    var elelist = Ext.query(".node");
    for (var i = 0; i < elelist.length; i++){
        elelist[i].style.fill = 'black';
        children = elelist[i].getElementsByTagName("*");
        for(var j = 0; j < children.length; j++)
            if (children[j].nodeName == 'polygon' || children[j].nodeName == 'ellipse')
                children[j].setAttribute("stroke","black");
    }
    /*
    elelist = Ext.query(".cluster")
    for (var i = 0; i < elelist.length; i++){
        children = elelist[i].getElementsByTagName("*");
        for(var j = 0; j < children.length; j++){
            if (children[j].nodeName == 'polygon' || children[j].nodeName == 'elli    pse')
                children[j].setAttribute("stroke","black");
            else if (children[j].nodeName == 'text')
                children[j].style.fill = "black";
        }
    }
    */
    els = Ext.query("ellipse");
    for(var i = 0; i < els.length; i++){
        els[i].setAttribute("stroke", "black");
    }

    els = Ext.query("polygon");
    for(var i = 0; i < els.length; i++){
        if(els[i].parentNode.className.baseVal != "graph")
            els[i].setAttribute("stroke", "black");
    }
    
    els = Ext.select("text");
    els.setStyle("fill", "black");
    //Reset edges
    var edges = Ext.query(".edge");
    for(var i = 0; i < edges.length; i++){
        el = edges[i].getElementsByTagName("title");
        edge = el[0].textContent;
        path = edges[i].getElementsByTagName("path")[0];
        path.setAttribute("stroke", "black");
        polygon = edges[i].getElementsByTagName("polygon")[0];
        polygon.setAttribute("stroke", "black");
        polygon.setAttribute("fill", "black");
    }
    polishSvg();
}

function graphScrollTo(Id){
    console.log(Id);
    var ele = document.getElementById(Id);
    var graph = document.getElementById('graph-panel-body');
    var eleRect = ele.getBoundingClientRect();
    var graphRect = graph.getBoundingClientRect();
    console.log(eleRect);
    console.log(graphRect);
    var xMargin = (eleRect.left + eleRect.right) / 2 - (graphRect.left + graphRect.right) / 2;
    var yMargin = (eleRect.top + eleRect.bottom) / 2 - (graphRect.top + graphRect.bottom) / 2;
    console.log(xMargin, yMargin);
    graph.scrollLeft += xMargin;
    graph.scrollTop += yMargin;
}

function selectBB(Id){
    var ele = Ext.get(Id);
    try{
        ele.scrollIntoView(Ext.get('graph-panel-body'), true);
        polygonSelector = ele.select("polygon");
        polygonSelector.set({"fill" : selected});
        textSelector = ele.select("text");
    }
    catch(err){console.log(err);}
}

// nodes: the list of nodes
function selectEdge(nodes){
    var edges = Ext.query(".edge");
    for(var i = 0; i < edges.length; i++){
        el = edges[i].getElementsByTagName("title");
        edge = el[0].textContent;
        vertex = edge.split("->");
        if(vertex.length != 2){
            console.log(vertex);
            console.log(edge);
        }
        else{
            var src = g_displayFunction + "*" + vertex[0];
            var dst = g_displayFunction + "*" + vertex[1];
            if (nodes.indexOf(src) != -1 && nodes.indexOf(dst) != -1){
                path = edges[i].getElementsByTagName("path")[0];
                path.setAttribute("stroke", selected);
                polygon = edges[i].getElementsByTagName("polygon")[0];
                polygon.setAttribute("stroke", selected);
                polygon.setAttribute("fill", selected);
            }
        }

    }
}

function selectGotoEdge(s, d){
    var edges = Ext.query(".edge");
    for(var i = 0; i < edges.length; i++){
        el = edges[i].getElementsByTagName("title");
        edge = el[0].textContent;
        vertex = edge.split("->");
        if(vertex.length != 2){
            console.log(vertex);
            console.log(edge);
        }
        else{
            var src = g_displayFunction + "*" + vertex[0];
            var dst = g_displayFunction + "*" + vertex[1];
            if (src == s && dst == d){
                path = edges[i].getElementsByTagName("path")[0];
                path.setAttribute("stroke", selected);
                polygon = edges[i].getElementsByTagName("polygon")[0];
                polygon.setAttribute("stroke", selected);
                polygon.setAttribute("fill", selected);
                break;
            }
        }

    }
}

function selectGraph(Id){
    try {
        ele = Ext.get(Id);
        polygonSelector = ele.select("polygon");
        polygonSelector.set({"stroke" : selected});
        ele.select("text").setStyle("fill", selected);
    }
    catch(err){}
}

// This is to adjust graph to make the element id big enough and scroll into view
function adjustGraph(Id){
    ele = Ext.get(Id);
    height = getGraphHeight(ele);
    panelHeight = Ext.getCmp("graph-panel").body.getHeight();
    currentRatio = panelHeight / height;
    if (currentRatio < 1 || currentRatio > 0.4)
        globalRatio *= zoomIn(currentRatio * 0.7);
    var domele = document.getElementById(Id);
    domele.scrollIntoView();
    return globalRatio;
}

function zoomIn(ratio){
    var svgEl = Ext.query("svg")[0];
    var widthString = svgEl.getAttribute('width');
    var width = parseInt(widthString.slice(0, widthString.indexOf('pt')));
    var heightString = svgEl.getAttribute('height');
    var height = parseInt(heightString.slice(0, heightString.indexOf('pt')));
    width = width * ratio;
    height = height * ratio;
    widthString = width.toString() + 'pt';
    heightString = height.toString() + 'pt';
    svgEl.setAttribute('height', heightString);
    svgEl.setAttribute('width', widthString);
    return ratio;
}

function highlightKeyNodes(){
    if("BB_Entry" in g_tree_svg)
      markBB(g_tree_svg["BB_Entry"], 'yellow');
    if("BB_Error" in g_tree_svg)
      markBB(g_tree_svg["BB_Error"], 'red');
    if("BB_Exit" in g_tree_svg)
      markBB(g_tree_svg["BB_Exit"], 'yellow');
}
