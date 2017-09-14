var defaultExpandedServices = [];
var defaultExpandedServers = [];
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
    var queryParamExpandedServers = queryParams["expandedServers"];
    if (queryParamExpandedServers != undefined){
        defaultExpandedServers = $.map(queryParamExpandedServers.split(","),function(item){return item.split("+").join(" ");});
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
    var templateContainer = $("#templateContainer");
    var serviceTemplate = templateContainer.find(".serviceTemplate").clone();
    var serverTemplate = templateContainer.find(".serverTemplate").clone();
    var checkTemplate = templateContainer.find(".checkTemplate").clone();
    var collapserTemplate = templateContainer.find(".collapserTemplate").clone();
    var pingerTemplate = templateContainer.find(".pingerTemplate").clone();
    var informationTemplate = templateContainer.find(".informationTemplate").clone();
    var furtherDetailTemplate = templateContainer.find(".furtherDetailTemplate").clone();
    templateContainer.remove();

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
    var getCollapser = function(){return collapserTemplate.clone();};
    var getCollapserClosed = function() {
        var elem = getCollapser();
        elem.find(".collapserState").text("+");
        return elem;
    }
    var getCollapserOpen = function() {
        var elem = getCollapser();
        elem.find(".collapserState").text("-");
        return elem;
    }
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
        var fdRoot = furtherDetailTemplate.clone();
        var innerDataSpan = fdRoot.find(".furtherDetailInner");
        innerDataSpan.addClass(className);
        if (hideData){
            innerDataSpan.addClass(furtherDetailClass);
        }
        innerDataSpan.attr("title",label);
        innerDataSpan.find(".serviceDataDescriptor").text(label);
        innerDataSpan.find(".furtherDetailValue").append(inner);
        return fdRoot;
    };
    var services = _.groupBy(jsonStructure,function(item){ return item.service; });
    rootNode.html(_.map(services,function(checksInService,serviceName){
        console.log("service",serviceName,checksInService);
        var serviceRootNode = serviceTemplate.clone();
        var serviceExpanded = false;
        var headerContainer = serviceRootNode.find(".serviceHeaderContainer");
        var serviceCollapser = headerContainer.find(".serviceCollapser");
        var serviceHeader = headerContainer.find(".serviceHeader");
        var serviceNameElement = serviceHeader.find(".serviceName");
        serviceNameElement.text(serviceName);
        var serviceContent = serviceRootNode.find(".serviceContent");
        var servers = _.groupBy(checksInService,function(item){ return item.server; });


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

        serviceContent.html(
            _.map(servers,function(checksInServer,serverName){
                console.log("server",serverName,checksInServer);
                var serverRootNode = serverTemplate.clone();
                var serverHeader = serverRootNode.find(".serverHeader");
                serverHeader.find(".serverName").text(serverName);
                var serverChecks = serverRootNode.find(".serverContent");

                var serverToggleable = serverRootNode.find('.'+toggleableClass);
                var serverHideable = serverRootNode.find('.'+hideableClass);

                var serverCollapser = serverRootNode.find(".serverCollapser");
                var serverExpanded = false;
                serverCollapser.html(getCollapserClosed());

                var expandServer = function(){
                    serverToggleable.addClass(toggledInClass).removeClass(toggledOutClass);
                    serverHideable.removeClass(hideableHiddenClass)
                    serverCollapser.html(getCollapserOpen());
                    serverExpanded = true;
                    pluginSystem.fireCommand('layoutChanged','core.expandServer');
                };
                var collapseServer = function(){
                    serverToggleable.addClass(toggledOutClass).removeClass(toggledInClass);
                    serverHideable.addClass(hideableHiddenClass)
                    serverCollapser.html(getCollapserClosed());
                    serverExpanded = false;
                    pluginSystem.fireCommand('layoutChanged','core.collapseServer');
                };
                serverCollapser.on('click',function(){
                    if (serverExpanded){
                        collapseServer();
                    } else {
                        expandServer();
                    }
                });
                serverToggleable.addClass(toggledOutClass).removeClass(toggledInClass);
                serverHideable.addClass(hideableHiddenClass);
                console.log("serverHideable",serverHideable);
                if (_.find(defaultExpandedServers,function(item){return item == serverName;}) != undefined){
                    expandServer();
                }

                serverChecks.html(_.map(checksInServer,function(check){
                    var checkNode = checkTemplate.clone();
                    checkNode.attr("id",check.id);
                    console.log("check: " + check.label + ", " + check.mode);

                    var collapser = checkNode.find(".checkCollapser");
                    collapser.html(getCollapserClosed());
                    var expanded = false;

                    if ('type' in check){
                        if (check.type == "pinger"){
                            var pingerRoot = pingerTemplate.clone();
                            var summary = pingerTemplate.find(".checkSummary");
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
                            summaryLine1.append(capacity);
                            var summaryLine2 = checkElem([],true,true);
                            var serviceClass = furtherDetail('Class','serviceClass',true,check.mode);
                            var lastChecked = furtherDetail('Last checked','serviceLastChecked',false,check.lastChecked);
                            var lastUp = furtherDetail('Last up','serviceLastUp',true,check.lastUp);
                            var frequency = furtherDetail('Frequency','servicePeriod',true,check.pollInterval);
                            summaryLine2.append(serviceClass).append(lastChecked).append(lastUp).append(frequency);
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
                    return checkNode;
                }));

                return serverRootNode;
            }));
        return serviceRootNode;//serviceNodes.push(serviceRootNode);
    }));
};
function updateCheck(newCheck){
    pluginSystem.fireCommand('dataChanged','core.updateCheck',newCheck);
};
function createCheck(obj){
    pluginSystem.fireCommand('createCheck','core.createCheck',obj);
};
function internalUpdateCheck(newCheck,targetNode){
    console.log("newCheck: " + newCheck["label"] + ", " + newCheck["mode"]);

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
