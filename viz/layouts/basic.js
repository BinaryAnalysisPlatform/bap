//
// Note that these are all defined as panel configs, rather than instantiated
// as panel objects.  You could just as easily do this instead:
//
// var absolute = Ext.create('Ext.Panel', { ... });
//
// However, by passing configs into the main container instead of objects, we can defer
// layout AND object instantiation until absolutely needed.  Since most of these panels
// won't be shown by default until requested, this will save us some processing
// time up front when initially rendering the page.
//
// Since all of these configs are being added into a layout container, they are
// automatically assumed to be panel configs, and so the xtype of 'panel' is
// implicit.  To define a config of some other type of component to be added into
// the layout, simply provide the appropriate xtype config explicitly.
//

function readFrom(filePath){
  xmlhttp = new XMLHttpRequest();
  xmlhttp.overrideMimeType('text/plain');
  xmlhttp.open("GET",filePath,false);
  xmlhttp.send(null);
  fileContent = xmlhttp.responseText;
  fileArray = fileContent.split('\n')
  n = fileArray.length;
  return fileContent;
}
function getStartGraph() {
    var svg = [];
    var store = new Ext.data.JsonStore({
        storeId: 'myStore',
        proxy: {
            type: 'ajax',
            url: 'expsum_for/function.json',
            reader: {
                type: 'json',
                root: 'functionlist',
                idProperty: 'id'
            }
        },
        fields: ['id', 'cft']
    });
    var svg = new Array();
    store.load(function(records, operation, success){
        var length = this.getCount();
        for (var i=0;i < length; i++){
            fileName = this.getAt(i).get('cft');
            cfg = readFrom("expsum_for/cft/" + fileName);
            //alert(cfg);
            result = Viz(cfg, "svg");
            var svgpush = [];
            svgpush.push("aaa");
            //alert(svgpush);
        }
    });
    console.log(store.data.items[0]);
    Ext.Object.each(store.data.items, function(record){
      console.log(store.getAt(0));
    });
    svg.push("bbb");
    console.log(svg[0]);
    // This is a fake CardLayout navigation function.  A real implementation would
    // likely be more sophisticated, with logic to validate navigation flow.  It will
    // be assigned next as the handling function for the buttons in the CardLayout example.
    var cardNav = function(incr){
        var l = Ext.getCmp('card-wizard-panel').getLayout();
        var i = l.activeItem.id.split('card-')[1];
        var next = parseInt(i, 10) + incr;
        l.setActiveItem(next);
        Ext.getCmp('card-prev').setDisabled(next===0);
        Ext.getCmp('card-next').setDisabled(next===2);
    };
    //var drawComponent = Ext.create('Ext.draw.Component', {
    //	    viewBox: false,
    //	    items: [{
    //		type: 'circle',
    //		fill: '#ffc',
    //		radius: 100,
    //		x: 100,
    //		y: 100
    //	    }]
    //	});
    //	Ext.create('Ext.Window', {
    //	    width: 230,
    //	    height: 230,
    //	    layout: 'fit',
    //	    items: [drawComponent]
    //	}).show();
    
    return {
        /*
         * ================  Start page config  =======================
         */
        // The default start page, also a simple example of a FitLayout.
        start: {
            id: 'start-graph-panel',
            title: 'Control Flow Graph',
            region: 'center',
            //layout: 'fit',
            autoScroll: true,
            bodyStyle: 'padding:25px; background:#fff;',
            contentEl: 'start-graph-div'  // pull existing content from the page
        }

        /*
         * ================  CardLayout config (TabPanel)  =======================
         */
        // Note that the TabPanel component uses an internal CardLayout -- it is not
        // something you have to explicitly configure.  However, it is still a perfect
        // example of how this layout style can be used in a complex component.
        /* cardTabs: {
            xtype: 'tabpanel',
            id: 'f-panel',
            activeTab: 0,
            style: 'background-color:#dfe8f6; ',
            defaults: {bodyStyle: 'padding:15px'},
            autoScroll: true,
            items:[{
                title: 'Control Frow Graph',
                html: svg['f-panel']
                //html: gethtml(id)
                //html: gethtml
                },{
                title: 'Loop Nesting Forests',
                html: 'Here will show LNF.'
                },{
                title: 'Control Flow Tree',
                html: ""
            }]
        }*/
    };
}




