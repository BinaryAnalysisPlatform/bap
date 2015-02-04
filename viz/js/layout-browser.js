Ext.Loader.setConfig({enabled: true});
Ext.Loader.setPath('Ext.ux.layout', 'js');

var TESTDIR = "data";

var getParams = document.URL.split('?');
var binaryName = Ext.urlDecode(getParams[1])['bin'];
var g_displayFunction = "", previousFunction="", displayNode, g_previousNode;

var fileFunctionList = TESTDIR + '/' + binaryName + '/' + binaryName + '.functionlist';
var fileJson = TESTDIR + '/' + binaryName + '/' + binaryName + '.json';
var globalRatio = 1;
var store, treePanel;
var g_tree_svg = {};
Ext.require([
    'Ext.tip.QuickTipManager',
    'Ext.container.Viewport',
    'Ext.layout.*',
    'Ext.form.Panel',
    'Ext.form.Label',
    'Ext.grid.*',
    'Ext.data.*',
    'Ext.tree.*',
    'Ext.selection.*',
    'Ext.tab.Panel',
    'Ext.ux.layout.Center'
]);

function jsonIdToGraphName(jsonId){
    index = jsonId.indexOf("*");
    return jsonId.substr(index+1, jsonId.length - index - 1);
}

/* Tree panel */
function getLeafNodes(record){
    var res = []
    for(var i = 0; i < record.childNodes.length; i++){
        if (!record.childNodes[i].get("leaf"))
            res = res.concat(getLeafNodes(record.childNodes[i]));
        else 
            res.push(record.childNodes[i].getId());
    }
    return res;
}

function getNoneLeafNodes(record){
    var res = [];
    var children = record.childNodes;
    for(var i = 0; i < children.length; i++){
        if (!children[i].get("leaf")){
            res.push(children[i].getId());
            res = res.concat(getNoneLeafNodes(children[i]));
        }
    }
    return res;
}

function getLastLeaf(record){
    if(record.previousSibling == null)
      return getLastLeaf(record.parentNode);
    else{
      var r = record.previousSibling;
      while (!r.get('leaf'))
        r = r.lastChild;
      return r.getId();
    }
}


/* Info panel*/
function generateInfo(){
    var store = new Ext.data.JsonStore({
        proxy: {
            type: 'ajax',
            url: fileFunctionList,
            reader: {
                type: 'json',
                root: 'binary'
            }
        },
        fields: ['name', {name: 'functionNumber', type: 'int'}, 'functions', 'startAddress', 'endAddress']
    });

    store.load(function(records, operation, success){
        binary = store.getAt(0);
        binaryName = binary.get("name");
        //binaryStrip = binary.get("strip");
        functionList = binary.get("functions");
        functionNumber = binary.get("functionNumber");

        var bd = document.getElementById('non-display');
        // Generate function information page
        for(var i = 0; i < functionNumber; i++){
            functionInfo = document.createElement("div");
            functionInfo.id = functionList[i].name + "-info";
            functionInfo.innerHTML = "<p>Function Name: " + functionList[i].name + "<br>Start Address: " + functionList[i].startAddress + "<br>End Address: " + functionList[i].endAddress + "</p>";
            bd.appendChild(functionInfo);
        }
        // Generate start page
        startInfo = document.createElement("div");
        startInfo.id = "start-info";
        //textContent = "<p> Binary Name: " + binaryName + "<br> Function Numbers: " + functionNumber + "<br> Stripped: " + binaryStrip + "</p>";
        textContent = "<p>Name: " + binaryName + "<br>Number of Functions: " + functionNumber + "</p>";
        startInfo.innerHTML = textContent;
        var infoPanel = document.getElementById("info-panel-body");
        infoPanel.innerHTML = textContent;
        bd.appendChild(startInfo);
        //return textContent;
    });
}

function infoPanelDisplay(displayFunction){
    Ext.getCmp('info-panel').body.update(Ext.getDom(displayFunction + '-info').innerHTML); //.slideIn('l', {stopAnimation:true,duration: 200});
}

/* Graph panel*/
function handleDbclick(e, t){
    e.preventDefault();
    globalRatio *= zoomIn(1.5);
}

function handleSubgraphClick(e, t){
    e.preventDefault();
    subgraphId = t.parentNode.id;
    var subgraphEl = Ext.get(subgraphId);
    var textEl = subgraphEl.last();
    subgraphId = textEl.dom.textContent;
    subgraphId = g_displayFunction + "*" + subgraphId;
    console.log(subgraphId);
    var path = treePanel.store.getNodeById(subgraphId).getPath();
    treePanel.selectPath(path);
    scrollTreeNode(subgraphId);
}

function handleNodeClick(e, t){
    e.preventDefault();
    nodeId = t.parentNode.id;
    var nodeEl = Ext.get(nodeId);
    var titleEl = nodeEl.first();
    var nodeId = g_displayFunction + "*" + titleEl.dom.textContent;
    //console.log("nodeID: " + nodeId);
    var treeNode = treePanel.getStore().getNodeById(nodeId);
    var path = treeNode.getPath();
    treePanel.selectPath(path);
    console.log(nodeId);
    scrollTreeNode(nodeId);

}

function selectGoto(nodeName){
    var index = nodeName.indexOf("|");
    // dst: function*BB_number
    var dst = nodeName.substr(0, index);
    console.log(dst);
    record = treePanel.store.getNodeById(nodeName);
    var src = getLastLeaf(record);
    nodeName = jsonIdToGraphName(nodeName);
    console.log(src);
    selectGotoEdge(src, dst);
    var srcBBId = g_tree_svg[jsonIdToGraphName(src)];
    var dstBBId = g_tree_svg[jsonIdToGraphName(dst)];
    var srcBBEle = document.getElementById(srcBBId);
    var dstBBEle = document.getElementById(dstBBId);
    selectBB(srcBBId);
    selectBB(dstBBId);
    //var left = min(srcBBEle.getBoundingClientRect().left, dstBBEle.getBoundingClientRect().left);
    //var top = min(srcBBEle.getBoundingClientRect().top, dstBBEle.getBoundingClientRect().top);
    //return left, top;
}

function graphPanelDisplay(functionName){
    cfg = readFrom(getCfgFileName(functionName));
    svg = Viz(cfg, "svg");
    Ext.getCmp('graph-panel').body.update("<div id='svg-panel' style='float: left; width: 100%'>" + svg + "</div>");
    polishSvg();

    //XXX: graph1 should be found by function
    globalRatio = 1;
    globalRatio = adjustGraph("graph1");

    Ext.EventManager.on("svg-panel", 'dblclick', handleDbclick);

    nodes = Ext.query(".node");
    for(var i = 0; i < nodes.length; i++){
        Ext.EventManager.on(nodes[i], 'click', handleNodeClick);
    }
    subgraphs = Ext.query(".cluster");
    for(var i = 0; i < subgraphs.length; i++){
        Ext.EventManager.on(subgraphs[i], 'click', handleSubgraphClick);
    }
}

// This a function to update all information for function
function updateAllPanels(f){
    previousFunction = g_displayFunction;
    g_displayFunction = f;
    // display function info
    graphPanelDisplay(g_displayFunction);
    infoPanelDisplay(g_displayFunction);
    tabDisplay(g_displayFunction, 'hil');
    tabDisplay(g_displayFunction, 'bil');
    dismTabDisplay(previousFunction,g_displayFunction);
}

//
// This is the main layout definition.
//
Ext.onReady(function(){
    Ext.tip.QuickTipManager.init();
    // This is to define the tree data model. It add new fields.
    Ext.define('Binary', {
        extend: 'Ext.data.Model',
        fields: [
            {name: 'text', type: 'string'},
            {name: 'function', type: 'boolean', defaultValue: null}
        ]
    })
    store = Ext.create('Ext.data.TreeStore', {
        model: 'Binary',
        root: {
            expanded: true
        },
        proxy: {
            type: 'ajax',
            url: fileJson
        }
    });
    // Go ahead and create the TreePanel now so that we can use it below
    treePanel = Ext.create('Ext.tree.Panel', {
        id: 'tree-panel',
        title: 'Function List',
        region:'north',
        split: true,
        height: '80%',
        minSize: 150,
        rootVisible: false,
        autoScroll: true,
        store: store,
        listeners: {
            afterlayout: function(container, layout, eOpts){
                var nd = this.getSelectionModel().getLastSelected();
                if (!Ext.isEmpty(nd) && this.getStore().initialLoad){
                    this.getStore.initialLoad = false;
                    this.getView().focusRow(
                        this.getView().store.indesOf(nd)
                    );
                }
            }
        }
    });
    // Assign the changeLayout function to be called on tree node click.
    treePanel.getSelectionModel().on('select', function(selModel, record) {
        var view = treePanel.getView();
        treePanel.getView().focusRow(treePanel.getView().store.indexOf(record)); // return
        scrollTreeNode(record.getId());
        //treePanel.getView().focusNode(record); // return
        //console.log(selModel);
        //.getView().focus();
        //treePanel.focus();
        if (record.get('root'))
            Ext.getCmp('info-panel').body.update(Ext.getDom('start-info').innerHTML);
        else if (record.get('function')){
            // Display a new function
            if (g_displayFunction != record.getId()){
                updateAllPanels(record.getId());
                // map the tree id with graph node id
                mapNode(g_tree_svg);
                highlightKeyNodes();
            }
            else {
                globalRatio = adjustGraph("graph1");
                resetGraph();
                highlightKeyNodes();
		resetTabs();
            }
        }
        else if (record.get('leaf')){
            selectFunction = getFunction(record);
            if (selectFunction != g_displayFunction){
              updateAllPanels(selectFunction);
              mapNode(g_tree_svg);
              highlightKeyNodes();
            }
            else {
                resetGraph();
                highlightKeyNodes();
            }
            nodeName = record.getId();
            if(nodeName.indexOf("|") > 0){
                selectGoto(nodeName);
		nodeName = jsonIdToGraphName(nodeName);
	    }
            else{
                nodeName = jsonIdToGraphName(nodeName);
                nodeId = g_tree_svg[nodeName];
                selectBB(nodeId);
                graphScrollTo(nodeId);
            }
            tabsNodeDisplay(g_previousNode, nodeName);
        }
        // select subgraph
        else{
            selectFunction = getFunction(record);
            if (selectFunction != g_displayFunction){
                updateAllPanels(selectFunction);
                mapNode(g_tree_svg);
                highlightKeyNodes();
            }
            resetGraph();
            highlightKeyNodes();
            // select nodes and sub-subgraphs
            bbNodes = getLeafNodes(record);
            console.log(bbNodes);
            for(var i = 0; i < bbNodes.length; i++){
                if(bbNodes[i].indexOf("|") > 0)
                    selectGoto(bbNodes[i]);
                nodeName = jsonIdToGraphName(bbNodes[i]);
                nodeId = g_tree_svg[nodeName];
                selectBB(nodeId);
            }

            // select other subgraph
            subgraphNodes = getNoneLeafNodes(record);
            console.log(subgraphNodes);
            for(var i = 0; i < subgraphNodes.length; i++){
                var subgraphName = jsonIdToGraphName(subgraphNodes[i]);
                var subgraphId = g_tree_svg[subgraphName];
                selectGraph(subgraphId);
            }
            // select this subgraph
            subgraphName = record.getId();
            subgraphName = jsonIdToGraphName(subgraphName);
            console.log(subgraphName);
            try{
                subgraphId = g_tree_svg[subgraphName];
                selectGraph(subgraphId);
                adjustGraph(subgraphId);
                graphScrollTo(subgraphId);
            }
            catch(err){}

            tabsNodeDisplay(g_previousNode, subgraphName);
            //select edges
            selectEdge(bbNodes);
        }
    });


    // This is the Details panel that contains the description for each example layout.
    var infoPanel = {
        //xtype: 'box',
        id: 'info-panel',
        title: 'Binary Information',
        region: 'center',
        bodyStyle: 'padding:25px; background:#fff',
        height: '20%',
        autoScroll: true,
        html: ''
    };
    // generate start page info (binary info) as well as function info
    generateInfo();

    // This is the right-top panel, which contains two tabs: disassembly tab and hil tab.
    var codePanel = {
        xtype: 'tabpanel',
        id: 'code-panel',
        activeTab: 0,
        deferredRender : false,
        border: false,
        region: 'center',
        split: true,
        bodyStyle: 'background:#fff;',
        //height: '80%',
        items:[{
            title: 'Hi-BIL',
            autoScroll: true,
            id: 'hil',
            html: '<iframe id="hil" src="" style="padding: 0px 0px 0px 10px; width: 100%; height: 100%;" seamless></iframe>'
         }, {
            id: 'bil',
            title: 'BAP IL',
            html: '<iframe id="bil" src="" style="padding: 0px 0px 0px 10px; width: 100%; height: 100%;" seamless></iframe>'
         }, {
            title: 'Disassembly',
            id: 'dism',
            html: '<iframe id="dism" src="data/' + binaryName + '/' + binaryName + '.html" style="padding: 0px 0px 0px 10px; width: 100%; height: 100%;" seamless></iframe>'
         }]
    };

    // This is the central panel, which contains svg panel and button panel.
    var graphPanel = {
        id: 'graph-panel',
        title: 'Control Flow Graph',
        region: 'center',
        bodyStyle: 'padding:35px; background: #fff',
        autoScroll: true,
        contentEl: 'start-graph-div'
    };
    
    window.document.title = binaryName + " - Phoenix2";
    //var graphView = document.createElement("div");
    //graphView.id = "graph-panel-body-view";
    //document.getElementById("graph-panel-body").appendChild(graphView);
    Ext.create('Ext.Viewport', {
        layout: 'border',
        title: window.document.title,
        items: [{
            layout: 'border',
            id: 'layout-browser-west',
            region:'west',
            border: false,
            split: true,
            margins: '2 0 5 5',
            width: '15%',
            minSize: 100,
            maxSize: 500,
            items: [treePanel, infoPanel]
        },{
            layout: 'border',
            id: 'layout-browser-east',
            region:'east',
            border: false,
            split:true,
            margins: '2 5 5 0',
            width: '40%',
            minSize: 100,
            maxSize: 600,
            items: codePanel
        },{
            layout: 'border',
            id: 'layout-browser-center',
            region: 'center',
            border: false,
            split: true,
            margins: '2 0 5 0',
            //cls: 'my-panel',
            //activeItem: 0,
            border: false,
            items: graphPanel
        }],
        renderTo: Ext.getBody()
    });
    
    var buttonPanel = document.createElement("div");
    buttonPanel.setAttribute("style", "position: absolute; width: 20%; top: 50px; right: 0px");
    buttonPanel.id = 'button-panel';
    //document.getElementById('graph-panel_header-body').setAttribute("style", "float: left; width: 80%");
    document.getElementById('graph-panel').appendChild(buttonPanel);
    Ext.create('Ext.Button', {
        //style: {margin: '10px', width: '90px', position: 'fixed', right: '580px', top: '150px'},
        style: {height: '40px', width: '40px', position: 'absolute', right: '80px'},
        text: null,
        iconCls: 'zoom-in',
        iconAlign: 'top',
        renderTo : Ext.get('button-panel'),
        scale: 'large',
        listeners: {click: function(){
            globalRatio *= zoomIn(1.2);
            graphScrollTo(g_tree_svg[g_previousNode]);
        }}
    });
    Ext.create('Ext.Button',{
        style: {height: '40px', width: '40px', position: 'absolute', right: '30px'},
        //style: {height: '40px', width: '40px', position: 'fixed', top: '100px', right: '850px'},
        text: null,
        iconCls: 'zoom-out',
        iconAlign: 'top',
        renderTo : Ext.get('button-panel'),
        scale: 'large',
        listeners: {click: function(){
            globalRatio *= zoomIn(1 / 1.2);
            console.log(g_previousNode);
            graphScrollTo(g_tree_svg[g_previousNode]);}}
    });
});
