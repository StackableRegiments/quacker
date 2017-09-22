var defaultExpandedServices = [];
var defaultExpandedChecks = [];
var jsonStructure = {};
pluginSystem.registerCommand('dataChanged',function(){},function(){});
pluginSystem.registerCommand('createCheck',function(){
    pluginSystem.suspendCommand('dataChanged');
    pluginSystem.suspendCommand('layoutChanged');
},function(){
    pluginSystem.resumeCommand('dataChanged');
    pluginSystem.resumeCommand('layoutChanged');
});
pluginSystem.registerCommand('removeCheck',function(){},function(){});
pluginSystem.registerCommand('checkCreated',function(){},function(){});
pluginSystem.registerCommand('layoutChanged',function(){},function(){});
$(function (){
    pluginSystem.suspendCommand('dataChanged');
    pluginSystem.suspendCommand('layoutChanged');
    pluginSystem.subscribe('dataChanged','core.internalUpdateCheck',function(obj){internalUpdateCheck(obj);});
    var getQueryParameters = function(){
        var partString = window.location.search.substr(1);
        var parts = partString.split("&");
        var qParams = {};
        if (parts.length == 0) {
            return qParams;
        }
        $.each(parts,function(index,item){
            var tuple = item.split("=");
            qParams[unescape(tuple[0])] = unescape(tuple[1]);
        })
        return qParams;
    };
    queryParams = getQueryParameters();
    var queryParamExpandedServices = queryParams["expandedServices"];
    if (queryParamExpandedServices != undefined){
        defaultExpandedServices = $.map(queryParamExpandedServices.split(","),function(item){return item.split("+").join(" ");});
    }
    var queryParamExpandedChecks = queryParams["expandedChecks"];
    if (queryParamExpandedChecks != undefined){
        defaultExpandedChecks = $.map(queryParamExpandedChecks.split(","),function(item){return item.split("+").join(" ");});
    }
    createChecks();
    pluginSystem.resumeCommand('dataChanged');
    pluginSystem.resumeCommand('layoutChanged');
});
var renderChecks = _.once(function(){
        var rootId = "#serverContainer";
        var rootNode = $(rootId);
        rootNode.html(renderSvg(rootId));
        var redraw = function () {
            rootNode.html(renderSvg(rootId));
            requestAnimationFrame(redraw);
        };
        requestAnimationFrame(redraw);
});
function updateCheck(newCheck){
    if ("id" in newCheck) {
        var oldCheck = jsonStructure[newCheck.id];
        if (oldCheck !== undefined) {
            jsonStructure[newCheck.id] = _.merge(oldCheck, newCheck);
        }
    }
    pluginSystem.fireCommand('dataChanged','core.updateCheck',newCheck);
}
function createChecks(newChecks){
    if (_.isArray(newChecks)){
        _.forEach(newChecks,function(check){
            createCheck(check);
        });
    }
    renderChecks();
}
function createCheck(newCheck){
    if ("id" in newCheck) {
        jsonStructure[newCheck.id] = newCheck;
    }
    pluginSystem.fireCommand('createCheck','core.createCheck',newCheck);
    renderChecks();
}
function removeCheck(checkId){
    delete jsonStructure[checkId];
    pluginSystem.fireCommand('removeCheck','core.removeCheck',checkId);
}
function internalUpdateCheck(newCheck,targetNode){
    //console.log("newCheck: " + newCheck["label"] + ", " + newCheck["mode"]);

    var rootNode = targetNode;
    var id = newCheck["id"];
    if (rootNode == undefined){
        rootNode = $("#"+id);
    }
    var label = newCheck["label"];
    var lastChecked = newCheck["now"];
    var statusCode = newCheck["statusCode"];
    var truncate = function(inputString,count){ return inputString.substr(0,count);};
    var statusClasses = newCheck["statusClass"];
    var why = newCheck["why"];
    var detail = newCheck["detail"];
    var tooltip = statusCode +": "+ label + " (@"+lastChecked+") : "+why;
    rootNode.find(".serviceCapacity").text(label);
    rootNode.find(".serviceLastChecked").text(newCheck["now"]);
    var statusNode = rootNode.find(".serviceStatus").attr("title",tooltip).text(statusCode);
    if (statusClasses["serverError"] == true){
        statusNode.addClass("serverError").removeClass("serverUnknown").removeClass("serverOk");
        rootNode.find(".serviceWhy").text(why);
        rootNode.find(".serviceDetail").text(detail);
    } else if (statusClasses["serverOk"] == true){
        statusNode.addClass("serverOk").removeClass("serverUnknown").removeClass("serverError");
        rootNode.find(".serviceWhy").text(truncate(why,500));
        rootNode.find(".serviceDetail").text(truncate(detail,500));
    }       else {
        statusNode.addClass("serverUnknown").removeClass("serverOk").removeClass("serverError");
        rootNode.find(".serviceWhy").text("");
        rootNode.find(".serviceDetail").text("");
    }
    rootNode.find(".serviceLastUp").text(newCheck["lastUp"]);
    rootNode.find(".serviceClass").text(newCheck["mode"]);
}
