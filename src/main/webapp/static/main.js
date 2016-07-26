var defaultExpandedServices = [];
var defaultExpandedChecks = [];
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
    createChecks();
    pluginSystem.resumeCommand('dataChanged');
    pluginSystem.resumeCommand('layoutChanged');
});
function createChecks(){
    var hideableClass = "hideable";
    var toggleableClass = "toggleable";
    var toggledInClass = "toggledIn";
    var toggledOutClass = "toggledOut";
    var hideableHiddenClass = "hideableHidden";
    var checkExpandableClass = "checkExpandable";
    var checkExpandedClass = "checkExpanded";
    var checkCollapsedClass = "checkCollapsed";
    var furtherDetailClass = "furtherDetail";
    var furtherDetailHiddenClass = "furtherDetailHidden";
    var furtherDetailVisibleClass = "furtherDetailVisible";
    var rootNode = $("#serverContainer");
    var getCollapserClosed = function(){return $("<span/>",{text:"[+]",'class':"collapser"})};
    var getCollapserOpen = function(){return $("<span/>",{text:"[-]",'class':"collapser"})};
    var serviceNodes = [];
    var elem = function(extraClasses,hideable,toggleable,inner){
        var classes = extraClasses;
        if (hideable){
            classes.push(hideableClass);
        }
        if (toggleable){
            classes.push(toggleableClass);
        }
        var returnObj = $("<span/>",{
            'class': _.reduce(classes,function(acc,item){ return acc + " " + item;},"")
        });
        if (inner != undefined){
            returnObj.append(inner);
        }
        return returnObj;
    };
    var checkElem = function(extraClasses,hideable,toggleable,inner){
        var classes = extraClasses;
        if (hideable){
            classes.push(checkExpandableClass);
            classes.push(hideableClass);
        }
        if (toggleable){
            classes.push(toggleableClass);
        }
        var returnObj = $("<span/>",{
            'class': _.reduce(classes,function(acc,item){ return acc + " " + item;},"")
        });
        if (inner != undefined){
            returnObj.append(inner);
        }
        return returnObj;
    };
    var furtherDetail = function(label,className,hideData,inner){
        var dataClasses = className;
        if (hideData){
            dataClasses = dataClasses + furtherDetailClass;
        }
        var innerObj = $("<span/>",{
            'class':dataClasses,
            'title':label
        });
        var returnObj = $("<span/>",{
            class:"detailContainer"
        }).append($("<span/>",{
            'class': 'serviceDataDescriptor '+ furtherDetailClass,
            'text':label
        })).append($("<span/>",{
            'class': furtherDetailClass,
            'text':': '
        })).append(innerObj);
        if (inner != undefined){
            innerObj.append(inner);
        }
        return returnObj;
    };
    var services = _.groupBy(jsonStructure,function(item){ return item.service; });
    _.forIn(services,function(checksInService,serviceName){
        var serviceRootNode = $("<div/>",{
            'class':'serviceGroup'
        });
        var serviceExpanded = false;
        var headerContainer = elem([],false,true);
        var serviceCollapser = $("<span/>",{
            'class':'serviceCollapser'
        });
        var serviceHeader = $("<span/>",{
            'class':'serviceHeader',
            'text':serviceName
        });
        var serviceContent = $("<span/>",{
            'class':'serviceContent'
        });
        serviceRootNode.append(headerContainer.append(serviceCollapser).append(serviceHeader)).append(serviceContent);
        var servers = _.groupBy(checksInService,function(item){ return item.server; });
        _.forIn(servers,function(checksInServer,serverName){
            var serverRootNode = elem(['serverGroup'],false,true);
            var serverHeader = elem(['serverHeader'],true,true,serverName);
            var serverChecks = $("<span/>",{
                'class':'serverChecks'
            });
            serverRootNode.append(serverHeader).append(serverChecks);
            _.forEach(checksInServer,function(check){
                var checkNode = $("<span/>",{
                    'class': _.reduce(['serviceCheck','check',toggleableClass],function(acc,item){return acc + ' ' + item;},''),
                    'id': check.id
                });
                var collapser = elem(['checkCollapser'],true,false);
                collapser.html(getCollapserClosed());
                var expanded = false;
                checkNode.append(collapser);
                if ('type' in check){
                    if (check.type == "pinger"){
                        var summary = $("<span/>",{
                            'class':'checkSummary'
                        });
                        var tooltip = check.lastStatusCode +": "+ check.label + " (@"+check.lastChecked+") : "+check.lastWhy;
                        var serviceStatus = furtherDetail('','serviceStatus',false,check.lastStatusCode).attr('title',tooltip);
                        switch (check.lastStatusCode){
                        case 'Y':
                            serviceStatus.addClass('serverOk');
                            break;
                        case 'N':
                            serviceStatus.addClass('serverError');
                            break;
                        default:
                        }
                        checkNode.prepend(serviceStatus);
                        var summaryContainer = elem([],true,false);
                        var summaryLine1 = checkElem([],false,false);
                        var capacity = furtherDetail('Purpose','serviceCapacity',true,check.label);
                        var serviceClass = furtherDetail('Class','serviceClass',true,check.mode);
                        summaryLine1.append(capacity).append(serviceClass);
                        var summaryLine2 = checkElem([],true,true);
                        var lastChecked = furtherDetail('Last checked','serviceLastChecked',false,check.lastChecked);
                        var lastUp = furtherDetail('Last up','serviceLastUp',true,check.lastUp);
                        var frequency = furtherDetail('Frequency','servicePeriod',true,check.pollInterval);
                        summaryLine2.append(lastChecked).append(lastUp).append(frequency);
                        summaryContainer.append(summaryLine1).append(summaryLine2);
                        summary.append(summaryContainer);
                        var detailContainer = checkElem([],true,true);
                        var checkDetail = $("<div/>",{
                            'class':'checkDetail '+checkExpandableClass
                        });
                        var message = checkElem(['serviceWhy','furtherDetail'],false,true,$("<span/>",{text:check.lastWhy}));
                        var detail = checkElem(['serviceDetail','furtherDetail'],false,true,$("<span/>",{text:check.lastDetail}));
                        detailContainer.append(checkDetail.append(message).append(detail));
                        checkNode.append(summary).append(detailContainer);
                    } else if (check.type == "information"){
                        var serviceStatus = elem([],true,false,"Information");
                        var header = furtherDetail('Information','serviceCapacity',true,check.label);
                        var information = checkElem(['information'],true,true,check.information);
                        checkNode.append(serviceStatus).append(checkElem([],true,false).append(checkElem([],false,true).append(header))).append(checkElem([],true,true).append(checkElem([],false,true).append(information)));
                    } else if (check.type == "htmlInformation"){
                        var serviceStatus = elem([],true,false,"Information");
                        var header = furtherDetail('Information','serviceCapacity',true,check.label);
                        var information = checkElem(['information'],true,true,check.information);
                        checkNode.append(serviceStatus).append(checkElem([],true,false).append(checkElem([],false,true).append(header))).append(checkElem([],true,true).append(checkElem([],false,true).append(information)));
                    } else if (check.type == "endpoints"){
                        var serviceStatus = elem([],true,false,"Endpoints");
                        var headerContainer = furtherDetail('Endpoint Information','serviceCapacity',true);
                        var endpointContainer = checkElem([],true,true);
                        var information = checkElem(['information'],true,true,check.information);
                        _.forEach(check.endpoints,function(ep){
                            var ep = checkElem([],true,true,$("<a/>",{
                                href:ep.url,
                                target:"_blank",
                                title:ep.description,
                                text:ep.name
                            }));
                            endpointContainer.append(ep);
                        });
                        checkNode.append(serviceStatus).append(checkElem([],true,false).append(checkElem([],false,true).append($("<div/>").append(headerContainer))).append(checkElem([],false,true).append($("<div/>").append(information)))).append($("<div/>").append(checkElem([],true,true).append(checkElem([],false,true).append(endpointContainer))));
                    } else if (check.type == "error"){
                        var info = elem([],false,true,"Errors");
                        var description = furtherDetail('Description','serviceCapacity',false,check.label);
                        var sourceItem = furtherDetail('Source','errorSource',false,check.source);
                        var expectedPeriod = furtherDetail('Expected period','errorPeriod',false,check.expectedPeriod);
                        var errorContainer = furtherDetail('Errors','errorList',false);
                        _.forEach(check.errors,function(item){
                            errorContainer.append($("<span/>",{
                                'class':'errorItem',
                                'text':item
                            }));
                        });
                        checkNode.append(info).append(checkElem([],true,false).append(checkElem([],false,true).append(description))).append(checkElem([],true,true).append(checkElem([],false,true).append(sourceItem)).append(checkElem([],false,true).append(expectedPeriod)).append(checkElem([],false,true).append(errorContainer)).append($("<hr/>")));
                    }
                }
                var hideable = checkNode.find('.'+checkExpandableClass);
                var toggleable = checkNode.find('.'+toggleableClass);
                var thisChecksFurtherDetails = checkNode.find('.'+furtherDetailClass);
                var collapseCheck = function(){
                    thisChecksFurtherDetails.addClass(furtherDetailHiddenClass).removeClass(furtherDetailVisibleClass);
                    toggleable.addClass(toggledOutClass).removeClass(toggledInClass);
                    hideable.addClass(hideableHiddenClass).addClass(checkCollapsedClass).removeClass(checkExpandedClass);
                    collapser.html(getCollapserClosed());
                    expanded = false;
                    pluginSystem.fireCommand('layoutChanged','core.collapseCheck');
                };
                var expandCheck = function(){
                    thisChecksFurtherDetails.addClass(furtherDetailVisibleClass).removeClass(furtherDetailHiddenClass);
                    toggleable.addClass(toggledInClass).removeClass(toggledOutClass);
                    hideable.removeClass(hideableHiddenClass).addClass(checkExpandedClass).removeClass(checkCollapsedClass);
                    collapser.html(getCollapserOpen());
                    expanded = true;
                    pluginSystem.fireCommand('layoutChanged','core.expandCheck');
                };
                collapser.on('click',function(){
                    if (expanded){
                        collapseCheck();
                    } else {
                        expandCheck();
                    }
                });
                thisChecksFurtherDetails.addClass(furtherDetailHiddenClass).removeClass(furtherDetailVisibleClass);
                hideable.addClass(hideableHiddenClass).addClass(checkCollapsedClass).removeClass(checkExpandedClass);
                toggleable.addClass(toggledOutClass).removeClass(toggledInClass);
                var checkCreatedInfo = {
                    checkData:check,
                    service:check.service,
                    server:check.server,
                    serviceNode:serviceRootNode,
                    serverNode:serverRootNode,
                    checkNode:checkNode
                };
                pluginSystem.fireCommand('checkCreated','core.createChecks',checkCreatedInfo);
                if (_.find(defaultExpandedChecks,function(item){return item == check.label;}) != undefined){
                    expandCheck();
                }
                serverChecks.append(checkNode);
            });
            serviceContent.append(serverRootNode);
        });
        var serviceToggleable = serviceRootNode.find('.'+toggleableClass);
        var serviceHideable = serviceRootNode.find('.'+hideableClass);
        serviceCollapser.html(getCollapserClosed());
        var expandService = function(){
            serviceToggleable.addClass(toggledInClass).removeClass(toggledOutClass);
            serviceHideable.removeClass(hideableHiddenClass)
            serviceCollapser.html(getCollapserOpen());
            serviceExpanded = true;
            pluginSystem.fireCommand('layoutChanged','core.expandService');
        };
        var collapseService = function(){
            serviceToggleable.addClass(toggledOutClass).removeClass(toggledInClass);
            serviceHideable.addClass(hideableHiddenClass)
            serviceCollapser.html(getCollapserClosed());
            serviceExpanded = false;
            pluginSystem.fireCommand('layoutChanged','core.collapseService');
        };
        serviceCollapser.on('click',function(){
            if (serviceExpanded){
                collapseService();
            } else {
                expandService();
            }
        });
        serviceToggleable.addClass(toggledOutClass).removeClass(toggledInClass);
        serviceHideable.addClass(hideableHiddenClass);
        if (_.find(defaultExpandedServices,function(item){return item == serviceName;}) != undefined){
            expandService();
        }
        serviceNodes.push(serviceRootNode);
    });
    rootNode.html(serviceNodes);
};
function updateCheck(newCheck){
    pluginSystem.fireCommand('dataChanged','core.updateCheck',newCheck);
};
function createCheck(obj){
    pluginSystem.fireCommand('createCheck','core.createCheck',obj);
};
function internalUpdateCheck(newCheck,targetNode){
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
