var defaultExpandedServices = [];
var defaultExpandedServers = [];
var defaultExpandedChecks = [];
var jsonStructure = {};
pluginSystem.registerCommand('renderCheckHtml',function(){},function(){});
pluginSystem.registerCommand('retile',function(){},function(){});
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
    var getQueryParameters = function(){
        var partString = window.location.search.substr(1);
        var parts = partString.split("&");
        var qParams = {};
        if (parts.length === 0) {
            return qParams;
        }
        $.each(parts,function(index,item){
            var tuple = item.split("=");
            qParams[unescape(tuple[0])] = unescape(tuple[1]);
        });
        return qParams;
    };
    var queryParams = getQueryParameters();
    var queryParamExpandedServices = queryParams["expandedServices"];
    if (queryParamExpandedServices !== undefined){
        defaultExpandedServices = $.map(queryParamExpandedServices.split(","),function(item){return item.split("+").join(" ");});
    }
    var queryParamExpandedServers = queryParams["expandedServers"];
    if (queryParamExpandedServers !== undefined){
        defaultExpandedServers = $.map(queryParamExpandedServers.split(","),function(item){return item.split("+").join(" ");});
    }
    var queryParamExpandedChecks = queryParams["expandedChecks"];
    if (queryParamExpandedChecks !== undefined){
        defaultExpandedChecks = $.map(queryParamExpandedChecks.split(","),function(item){return item.split("+").join(" ");});
    }
    renderChecks();
    pluginSystem.resumeCommand('dataChanged');
    pluginSystem.resumeCommand('layoutChanged');
});
var structureByServices = function(structure){
    return _.groupBy(structure, function (check) {
        return "All";
    });
};
var paused = false;
var renderChecks = _.once(function(){
	$(function(){
		var containerRootNode = $("#dashboardServerContainer");
		var redraw = function () {
			if (!paused){
				var serviceStructure = structureByServices(jsonStructure);
				_.forEach(serviceStructure, function(checks,serviceName){
						var serviceIdOuter = "serviceOuter_" + serviceName;
						var serviceNode = containerRootNode.find("#"+serviceIdOuter);
						if (serviceNode[0] === undefined){
								serviceNode = $("<div/>",{id:serviceIdOuter,class:"serviceOuter"});
								containerRootNode.append(serviceNode);
						}
						pluginSystem.fireCommand('renderCheckHtml','core.renderChecks',serviceNode,checks);
						pluginSystem.fireCommand('retile','core.renderChecks');
				});
			}
			requestAnimationFrame(redraw);
		};
    requestAnimationFrame(redraw);
	});
});

var renderHistoricalChecks = _.once(function(){
  console.log("render called");
});

var stripCheckHistory = function(check){
  delete check.history;
  return check;
}
var maxHistoryItems = 5;
function updateCheck(newCheck){
    if ("id" in newCheck) {
        var oldCheck = jsonStructure[newCheck.id];
        if (oldCheck !== undefined) {
            // console.log("updating check:",oldCheck,newCheck);
            var cached = _.cloneDeep(oldCheck);
            var merged = _.merge(oldCheck, newCheck);
            var oldChecks = _.orderBy(_.concat([stripCheckHistory(_.cloneDeep(cached))],oldCheck.history || []),["lastCheck"],["desc"]);
            var newHistory = _.take(oldChecks,maxHistoryItems - 1); //taking one less, now that we'll be putting ourselves into the listing for subsequent rendering.  This isn't the right point to fix it at, but it'll do for the moment.
            merged.history = newHistory;
            jsonStructure[newCheck.id] = merged;
            if (cached != merged){
                merged.dirty = true;
            }
        }
    }
    pluginSystem.fireCommand('dataChanged','core.updateCheck',newCheck);
}
function createHistoricalChecks(newChecks){
  if (_.isArray(newChecks)){
    _.forEach(newChecks,function(check){
      createHistoricalCheck(check);
    });
  }
  renderHistoricalChecks();
}
function createChecks(newChecks){
    if (_.isArray(newChecks)){
        _.forEach(newChecks,function(check){
            createCheck(check);
        });
    }
    renderChecks();
}
function createHistoricalCheck(newCheck){
  pluginSystem.fireCommand('createHistoricalCheck','core.createHistoricalCheck',newCheck);
  console.log('received historical item',newCheck);
}
function createCheck(newCheck){
    if ("id" in newCheck) {
        jsonStructure[newCheck.id] = newCheck;
        newCheck.history = _.orderBy(newCheck.history,["lastCheck"],["desc"]);
        pluginSystem.fireCommand('createCheck','core.createCheck',newCheck);
        renderChecks();
    }
}
function removeCheck(checkId){
    delete jsonStructure[checkId];
    pluginSystem.fireCommand('removeCheck','core.removeCheck',checkId);
}
var isDevMode = false;
function setDevMode(devMode){
    isDevMode = devMode;
}
