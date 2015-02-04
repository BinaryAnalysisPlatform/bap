function getFunction(record){
    t = record;
    while(!t.get("function"))
        t = t.parentNode;
    return t.getId();
}

function scrollTreeNode(Id){
    console.log(Id);
    var treeNode = treePanel.getStore().getNodeById(Id);
    console.log(treeNode);
    var treeNodeEle = treePanel.getView().getNode(treeNode);
    console.log(treeNodeEle);
    var text = treeNodeEle.getElementsByTagName('span')[0];
    var icon = text.previousSibling.previousSibling;
    var tree = document.getElementById('tree-panel-body');
    var treeView = tree.childNodes[0];
    console.log(text.getBoundingClientRect().right);
    console.log(treeView.getBoundingClientRect().right);
    if(text.getBoundingClientRect().right > treeView.getBoundingClientRect().right){
        xMargin = icon.getBoundingClientRect().left - treeView.getBoundingClientRect().left;
        console.log(xMargin);
        treeView.scrollLeft += xMargin;
    }
    
}

