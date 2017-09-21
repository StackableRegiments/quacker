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
    var rootId = "#serverContainer";
    var rootNode = $(rootId);
    var getCollapserClosed = function(){return $("<span/>",{text:"[+]",'class':"collapser"})};
    var getCollapserOpen = function(){return $("<span/>",{text:"[-]",'class':"collapser"})};
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
        })).append("&nbsp;&nbsp;").append(innerObj);
        if (inner != undefined){
            innerObj.append(inner);
        }
        return returnObj;
    };
    var renderSvg = function(){
        var svg = d3.select(rootId).append("svg");
        var data = _.toPairs(_.groupBy(jsonStructure,function(item){ return item.service; }));
        var dGroup = {
            width:200,
            height:200
        };
        var services = svg
            .selectAll(".service")
            .data(data)
            .enter().append("g")
            .attr("class","service")
            .attr("height",dGroup.height)
            .attr("width",dGroup.width)
            .attr("transform",function(d,i){
                return sprintf("translate(%s,%s)",i * dGroup.width, 0);
            });
        var serviceLabel = services.append("text")
            .text(function(d){
                return _.size(d[1]);
            })
            .attr("text-anchor","middle")
            .attr("x",dGroup.width/2)
            .attr("y",dGroup.height/2);
        var servers = services
            .selectAll(".server")
            .data(function(d){
                return d[1];
            })
            .enter().append("g")
            .attr("class","server");
        var labels = servers
            .append("text")
            .attr("class","label")
            .attr("x",60)
            .attr("y",function(d,i){
                return dGroup.height/2 + i * 30;
            })
            .text(function(d){
                return d.label;
            });
        var rings = servers
            .append("g")
            .data([30,60,90])
            .append("circle")
            .attr("cx",dGroup.width/2)
            .attr("cy",dGroup.height/2)
            .attr("r",function(d){return d})
            .attr("stroke","black")
            .attr("fill","none")
            .attr("stroke-width",2);
    };
    var renderHtml = function() {
        var services = _.groupBy(jsonStructure, function (item) {
            return item.service;
        });
        var serviceNodes = [];
        rootNode.html(_.map(services, function (checksInService, serviceName) {
            var serviceRootNode = $("<div/>", {
                'class': 'serviceGroup'
            });
            var serviceExpanded = false;
            var headerContainer = elem([], false, true);
            var serviceCollapser = $("<span/>", {
                'class': 'serviceCollapser'
            });
            var serviceHeader = $("<span/>", {
                'class': 'serviceHeader',
                'text': serviceName
            });
            var serviceContent = $("<span/>", {
                'class': 'serviceContent'
            });
            serviceRootNode.append(headerContainer.append(serviceCollapser).append(serviceHeader)).append(serviceContent);
            var servers = _.groupBy(checksInService, function (item) {
                return item.server;
            });
            serviceContent.html(_.map(servers, function (checksInServer, serverName) {
                // console.log("server",serverName,checksInServer);
                var serverRootNode = serverTemplate.clone();
                var serverHeader = serverRootNode.find(".serverHeader");
                serverHeader.find(".serverName").text(serverName);
                var serverChecks = serverRootNode.find(".serverContent");

                var serverToggleable = serverRootNode.find('.' + toggleableClass);
                var serverHideable = serverRootNode.find('.' + hideableClass);

                var serverCollapser = serverRootNode.find(".serverCollapser");
                var serverExpanded = false;
                serverCollapser.html(getCollapserClosed());

                var expandServer = function () {
                    serverToggleable.addClass(toggledInClass).removeClass(toggledOutClass);
                    serverHideable.removeClass(hideableHiddenClass);
                    serverCollapser.html(getCollapserOpen());
                    serverExpanded = true;
                    pluginSystem.fireCommand('layoutChanged', 'core.expandServer');
                };
                var collapseServer = function () {
                    serverToggleable.addClass(toggledOutClass).removeClass(toggledInClass);
                    serverHideable.addClass(hideableHiddenClass);
                    serverCollapser.html(getCollapserClosed());
                    serverExpanded = false;
                    pluginSystem.fireCommand('layoutChanged', 'core.collapseServer');
                };
                serverCollapser.on('click', function () {
                    if (serverExpanded) {
                        collapseServer();
                    } else {
                        expandServer();
                    }
                });
                serverToggleable.addClass(toggledOutClass).removeClass(toggledInClass);
                serverHideable.addClass(hideableHiddenClass);
                if (_.find(defaultExpandedServers, function (item) {
                        return item == serverName;
                    }) != undefined) {
                    expandServer();
                }

                serverChecks.html(_.map(checksInServer, function (check) {
                    var checkNode = checkTemplate.clone();
                    checkNode.attr("id", check.id);
                    // console.log("check: " + check.label + ", " + check.mode);

                    var collapser = checkNode.find(".checkCollapser");
                    collapser.html(getCollapserClosed());
                    var expanded = false;

                    if ('type' in check) {
                        if (check.type == "pinger") {
                            var detailContainer = checkDetailContainerTemplate.clone();
                            var tooltip = check.lastStatusCode + ": " + check.label + " (@" + check.lastChecked + ") : " + check.lastWhy;
                            detailContainer.attr('title', tooltip);

                            var checkDetail = detailContainer.find(".checkDetail");
                            checkDetail.find(".serviceWhy").append($("<span/>", {text: check.lastWhy}));
                            checkDetail.find(".serviceDetail").append($("<span/>", {text: check.lastDetail}));
                            checkNode.append(detailContainer);

                            var pingerNode = pingerTemplate.clone();
                            var checkStatus = furtherDetail('', 'checkStatus', false, check.lastStatusCode);
                            switch (check.lastStatusCode) {
                                case 'Y':
                                    checkStatus.addClass('serverOk');
                                    break;
                                case 'N':
                                    checkStatus.addClass('serverError');
                                    break;
                                default:
                            }
                            pingerNode.prepend(checkStatus);

                            var summaryContainer = pingerNode.find(".pingerSummary");
                            var summaryLine1 = summaryContainer.find(".summaryLine1");
                            var capacity = furtherDetail('Purpose', 'serviceCapacity', true, check.label);
                            summaryLine1.append(capacity);
                            var summaryLine2 = summaryContainer.find(".summaryLine2");
                            var serviceClass = furtherDetail('Class', 'serviceClass', true, check.mode);
                            var lastChecked = furtherDetail('Last checked', 'serviceLastChecked', false, check.lastChecked);
                            var lastUp = furtherDetail('Last up', 'serviceLastUp', true, check.lastUp);
                            var frequency = furtherDetail('Frequency', 'servicePeriod', true, check.pollInterval);
                            summaryLine2.append(serviceClass).append(lastChecked).append(lastUp).append(frequency);

                            checkNode.append(pingerNode);
                        } else if (check.type == "information") {
                            var informationNode = informationTemplate.clone();
                            var serviceStatus = informationNode.find(".serviceStatus");
                            var header = furtherDetail('Information', 'serviceCapacity', true, check.label);
                            var information = checkElem(['information'], true, true, check.information);
                            serviceStatus.append(checkElem([], true, false).append(checkElem([], false, true).append(header))).append(checkElem([], true, true).append(checkElem([], false, true).append(information)));
                        } else if (check.type == "htmlInformation") {
                            var serviceStatus = elem([], true, false, "Information");
                            var header = furtherDetail('Information', 'serviceCapacity', true, check.label);
                            var information = checkElem(['information'], true, true, check.information);
                            checkNode.append(serviceStatus).append(checkElem([], true, false).append(checkElem([], false, true).append(header))).append(checkElem([], true, true).append(checkElem([], false, true).append(information)));
                        } else if (check.type == "endpoints") {
                            var serviceStatus = elem([], true, false, "Endpoints");
                            var headerContainer = furtherDetail('Endpoint Information', 'serviceCapacity', true);
                            var endpointContainer = checkElem([], true, true);
                            var information = checkElem(['information'], true, true, check.information);
                            _.forEach(check.endpoints, function (ep) {
                                var ep = checkElem([], true, true, $("<a/>", {
                                    href: ep.url,
                                    target: "_blank",
                                    title: ep.description,
                                    text: ep.name
                                }));
                                endpointContainer.append(ep);
                            });
                            checkNode.append(serviceStatus).append(checkElem([], true, false).append(checkElem([], false, true).append($("<div/>").append(headerContainer))).append(checkElem([], false, true).append($("<div/>").append(information)))).append($("<div/>").append(checkElem([], true, true).append(checkElem([], false, true).append(endpointContainer))));
                        } else if (check.type == "error") {
                            var info = elem([], false, true, "Errors");
                            var description = furtherDetail('Description', 'serviceCapacity', false, check.label);
                            var sourceItem = furtherDetail('Source', 'errorSource', false, check.source);
                            var expectedPeriod = furtherDetail('Expected period', 'errorPeriod', false, check.expectedPeriod);
                            var errorContainer = furtherDetail('Errors', 'errorList', false);
                            _.forEach(check.errors, function (item) {
                                errorContainer.append($("<span/>", {
                                    'class': 'errorItem',
                                    'text': item
                                }));
                            });
                            checkNode.append(info).append(checkElem([], true, false).append(checkElem([], false, true).append(description))).append(checkElem([], true, true).append(checkElem([], false, true).append(sourceItem)).append(checkElem([], false, true).append(expectedPeriod)).append(checkElem([], false, true).append(errorContainer)).append($("<hr/>")));
                        }
                    }
                    var hideable = checkNode.find('.' + checkExpandableClass);
                    var toggleable = checkNode.find('.' + toggleableClass);
                    var thisChecksFurtherDetails = checkNode.find('.' + furtherDetailClass);
                    var collapseCheck = function () {
                        thisChecksFurtherDetails.addClass(furtherDetailHiddenClass).removeClass(furtherDetailVisibleClass);
                        toggleable.addClass(toggledOutClass).removeClass(toggledInClass);
                        hideable.addClass(hideableHiddenClass).addClass(checkCollapsedClass).removeClass(checkExpandedClass);
                        collapser.html(getCollapserClosed());
                        expanded = false;
                        pluginSystem.fireCommand('layoutChanged', 'core.collapseCheck');
                    };
                    var expandCheck = function () {
                        thisChecksFurtherDetails.addClass(furtherDetailVisibleClass).removeClass(furtherDetailHiddenClass);
                        toggleable.addClass(toggledInClass).removeClass(toggledOutClass);
                        hideable.removeClass(hideableHiddenClass).addClass(checkExpandedClass).removeClass(checkCollapsedClass);
                        collapser.html(getCollapserOpen());
                        expanded = true;
                        pluginSystem.fireCommand('layoutChanged', 'core.expandCheck');
                    };
                    collapser.on('click', function () {
                        if (expanded) {
                            collapseCheck();
                        } else {
                            expandCheck();
                        }
                    });
                    thisChecksFurtherDetails.addClass(furtherDetailHiddenClass).removeClass(furtherDetailVisibleClass);
                    hideable.addClass(hideableHiddenClass).addClass(checkCollapsedClass).removeClass(checkExpandedClass);
                    toggleable.addClass(toggledOutClass).removeClass(toggledInClass);
                    var checkCreatedInfo = {
                        checkData: check,
                        service: check.service,
                        server: check.server,
                        serviceNode: serviceRootNode,
                        serverNode: serverRootNode,
                        checkNode: checkNode
                    };
                    pluginSystem.fireCommand('checkCreated', 'core.createChecks', checkCreatedInfo);
                    if (_.find(defaultExpandedChecks, function (item) {
                            return item == check.label;
                        }) != undefined) {
                        expandCheck();
                    }
                    return checkNode;
                })); // good

                return serverRootNode;
            }));
            return serviceRootNode;
        }));
    }
    rootNode.html(renderSvg());
};
function updateCheck(newCheck){
    pluginSystem.fireCommand('dataChanged','core.updateCheck',newCheck);
}
function createCheck(obj){
    pluginSystem.fireCommand('createCheck','core.createCheck',obj);
}
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
