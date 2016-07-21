var defaultExpandedServices = [];
var defaultExpandedChecks = [];
var checkHistory = {
};
pluginSystem.registerCommand('dataChanged',function(){},function(){});
pluginSystem.registerCommand('createCheck',function(){
    pluginSystem.suspendCommand('dataChanged');
    pluginSystem.suspendCommand('layoutChanged');
},function(){
    pluginSystem.resumeCommand('dataChanged');
    pluginSystem.resumeCommand('layoutChanged');
});
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
    pluginSystem.resumeCommand('dataChanged');
    pluginSystem.resumeCommand('layoutChanged');
    glassCanvas = $("#glass");
    glass = glassCanvas[0].getContext("2d");
    render();
});
var checkStates = {};
var sorted = function(obj){
    return _.sortBy(obj,["lastStatusCode","service","server"]);
};
function render(){
    var w = $(window);
    var fullWidth = w.width();
    var fullHeight = w.height();
    glassCanvas.attr("width",fullWidth);
    glassCanvas.attr("height",fullHeight);
    var padding = {
        x:fullWidth * 0.1,
        y:fullHeight * 0.1
    };
    var inset = {
        x:padding.x / 2,
        y:padding.y / 2
    };
    var viewport = {
        x:inset.x,
        y:inset.y,
        width:fullWidth - padding.x,
        height:fullHeight - padding.y
    };
    var columns = 7;
    var nodeWidth = Math.floor(viewport.width / columns);
    var nodeHeight = nodeWidth * (Math.sqrt(3) / 2);
    var xStep = nodeWidth * 3/4;
    var yStep = nodeHeight;
    var stamp = (function(){
        var x = viewport.x;
        var y = viewport.y;
        var even = true;
        var center = {x:nodeWidth/2,y:nodeHeight/2};
        var radius = nodeWidth / 2;
        var sides = 6;
        var vertices = _.map(_.range(sides),function(i){
            return {
                x:center.x + radius * Math.cos(i * 2 * Math.PI / sides),
                y:center.y + radius * Math.sin(i * 2 * Math.PI / sides)
            }
        });
        var start = vertices[0];
        return {
            skip:function(){
                x = viewport.x;
                even = true;
                y += yStep;
            },
            render:function(check){
                glass.save();
                glass.translate(x,y);
                glass.beginPath();
                glass.moveTo(start.x,start.y);

                _.forEach(vertices,function(vertex){
                    glass.lineTo(vertex.x,vertex.y);
                });
                glass.lineTo(start.x,start.y);
                glass.strokeStyle = "#000000";
                glass.lineWidth = 1;
                glass.stroke();
                switch(check.statusCode || check.lastStatusCode){
                case "Y": glass.fillStyle = "#00FF00";break;
                case "N": glass.fillStyle = "#FF0000";break;
                case "?": glass.fillStyle = "#FFFF00";break;
                default:break;
                }
                glass.fill();
                x += xStep * 2;
                if(x >= viewport.width){//Carriage return
                    even = !even;
                    x = viewport.x + (even ? 0 : xStep);
                    y += yStep / 2;
                }
                glass.fillStyle = "#000000";

                if(viewport.width > 1500) glass.font = "16px Advent Pro";
                else if(viewport.width > 1000) glass.font = "12px Advent Pro";
                else if(viewport.width > 400) glass.font = "8px Advent Pro";
		else glass.font = "6px Advent Pro";

                glass.textBaseline = "bottom";
                glass.textAlign = "center";
                var textRoot = {x:start.x,y:start.y - radius/3};
                glass.fillText(check.label, start.x - radius, textRoot.y);
                glass.fillText(check.service, start.x - radius, textRoot.y + radius / 3);
                glass.fillText(check.mode, start.x - radius, textRoot.y + radius / 3 * 2);
                glass.restore();
            }
        };
    })();
    _.forIn(_.groupBy(jsonStructure,'service'),function(checksInService,serviceName){
        _.forIn(_.groupBy(sorted(checksInService),'server'),function(checksInServer,serverName){
            _.forIn(checksInServer,function(check){
                stamp.render(check);
            });
        });
        stamp.skip();
    });
    requestAnimationFrame(render);
}
function updateCheck(newCheck){
    pluginSystem.fireCommand('dataChanged','core.updateCheck',newCheck);
};
function createCheck(obj){
    pluginSystem.fireCommand('createCheck','core.createCheck',obj);
};
function internalUpdateCheck(newCheck,targetNode){
    console.log("internalUpdateCheck",newCheck);
    if(!newCheck.statusCode) newCheck.statusCode = newCheck.lastStatusCode;
    jsonStructure[newCheck.id] = newCheck;
    var history = checkHistory[newCheck.id];
    if(!history){
        history = checkHistory[newCheck.id] = {};
    }
    var then = history.lastSeen;
    var now = Date.now();
    if(then){
        history.observedInterval = now - then;
        history.timer = history.observedInterval;
        console.log("Observed interval",history.timer);
    }
    history.lastSeen = now;
}
