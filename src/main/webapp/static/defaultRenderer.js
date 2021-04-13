$(function(){
	pluginSystem.suspendCommand('dataChanged');
	pluginSystem.suspendCommand('layoutChanged');

	pluginSystem.subscribe('dataChanged','defaultRenderer.internalUpdateCheck',function(obj){return internalUpdateCheck(obj);});

	pluginSystem.resumeCommand('dataChanged');
	pluginSystem.resumeCommand('layoutChanged');

	pluginSystem.subscribe('retile','defaultRenderer.retile',function() {
			$('.serviceOuter').masonry({
				itemSelector: '.checkSummary'
				// columnWidth: 200
			});
	//    console.log("Retiled");
	});

	pluginSystem.subscribe('renderCheckHtml','defaultRenderer.renderChecks',function(serviceNode,checks){
		return renderHtml(serviceNode,checks);
	});

	function internalUpdateCheck(newCheck,targetNode){
    var rootNode = targetNode;
    var id = newCheck["id"];
    if (rootNode === undefined){
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
    if (statusClasses["serverError"] === true) {
        statusNode.addClass("serverError").removeClass("serverUnknown").removeClass("serverOk");
        rootNode.find(".serviceWhy").text(why);
        rootNode.find(".serviceDetail").text(detail);
    } else if (statusClasses["serverOk"] === true) {
        statusNode.addClass("serverOk").removeClass("serverUnknown").removeClass("serverError");
        rootNode.find(".serviceWhy").text(truncate(why,500));
        rootNode.find(".serviceDetail").text(truncate(detail,500));
    } else {
        statusNode.addClass("serverUnknown").removeClass("serverOk").removeClass("serverError");
        rootNode.find(".serviceWhy").text("");
        rootNode.find(".serviceDetail").text("");
    }
    rootNode.find(".serviceLastUp").text(newCheck["lastUp"]);
    rootNode.find(".serviceClass").text(newCheck["mode"]);
	}

	var TimeSpanFormatter = (function(){
		var timeFormats = [
			{unit:"millisecond",value:1000},
			{unit:"second",value:60},
			{unit:"minute",value:60},
			{unit:"hour",value:24},
			{unit:"day",value:7},
			{unit:"week",value:52}
		];
		var comprehendTimeSpanFunc = function(timespan){
			var ts = timespan;
			var output = [];
			_.forEach(timeFormats,function(tf){
				if (ts > 0){
					var c = ts % tf.value;
					ts = ((ts - c) / tf.value);
					if (c > 0){
						output.push({unit:tf.unit,value:c});
					}
				}
			});
			return output;
		};
		var formatTimeSpanFunc = function(timePeriods){
			return _.map(_.reverse(timePeriods),function(item){
				return item.value + " " + item.unit + (item.value > 1 ? "s" : "");
			}).join(", ");
		};
		return {
			formatTimeSpan:function(timespan){
				return formatTimeSpanFunc(comprehendTimeSpanFunc(timespan));
			}
		};
	})();

	var renderCheckSvg = function(checkElem, allChecks) {
    var dGroup = {
        width: 180,
        height: 180
    };
    var ringRadius = 75;
    var orbitingContainerWidth = 25;
    var orbitingContainerHeight = 25;
    var dotRadius = 10;
    var indicatorTextOffsetX = 0;
    var indicatorTextOffsetY = 4;
    var historiesWidth = 27;
    var historiesHeight = 102;
    var historyContainerWidth = 25;
    var historyContainerHeight = 25;
    var historyContainerPositionX = 0;
    var historyContainerPositionY = 0;
    var historyTextOffsetX = -1.5;
    var historyTextOffsetY = 2;
    var historyDurationOffsetX = 12;

    var constructIdentity = function (inString) {
        return _.replace(inString, " ", "_");
    };
    var svg = d3.select(checkElem[0]).append("svg");

    var gradient = svg.append("defs")
        .append("linearGradient")
        .attr("id","fadeRight")
        .attr("x1","100%")
        .attr("y1","0%")
        .attr("x2","0%")
        .attr("y2","0%");
    gradient
        .append("stop")
        .attr("offset","10%")
        .attr("class","historyElemGradientStart");
    gradient
        .append("stop")
        .attr("offset","75%")
        .attr("class","historyElemGradientEnd");

    var data = _.toPairs(_.mapValues(_.groupBy(allChecks, function (item) {
        return item.service;
    }), function (v, k) {
        return _.toPairs(_.groupBy(v, function (vi, vk) {
            return vi.server;
        }));
    }));

    var checks = svg.attr("viewBox",sprintf("0 0 %s %s", dGroup.width, dGroup.height))
//        .attr("preserveAspectRatio","XMidYMid meet")
        .selectAll(".check")
        .data(data)
        .enter().append("g")
        .attr("class", "check")
        .attr("id", function (d, i) {
            return constructIdentity("check_" + d[0]);
        })
        .attr("height", dGroup.height)
        .attr("width", dGroup.width)
        .attr("transform", function (d, i) {
            return sprintf("translate(%s,%s)", i * dGroup.width, 0);
        });

    var calcRingRadius = function(check){
        return ringRadius;
    };
    var calcRingCenterX = function(check, checkIndex) {
        return ((dGroup.width / 2) * checkIndex) + calcRingRadius(check) + dotRadius;
    };
    var calcRingCenterY = function(check, checkIndex) {
        return dGroup.height / 2;
    };
    var calcIndicatorProportion = function(d) {
        if("lastCheck" in d && "period" in d) {
            var now = new Date().getTime();
            return (now - d.lastCheck) / d.period;
        } else {
            return 0;
        }
    };
    var calcIndicatorAttributes = function(d,ignoreNextCheck) {
        // console.log("Calcing color",d,d.lastCheck,d.status,d.label);
        if("lastCheck" in d && "period" in d) {
            var nextCheck = (d.lastCheck + d.period);
            if (d.lastCheck == 0 || (!ignoreNextCheck && new Date().getTime() > nextCheck)) {
                // Stale (after next expected check time). A plug (disconnected).
                return {dotColor:"lightgrey",text:"\uf1e6",textColor:"grey",circleColor:"grey"};
            }
        }
        if("status" in d) {
            if (d.status == true) {
                // Check success. A tick.
                return {dotColor:"lightgreen",text:"\uf00c",textColor:"black",circleColor:"grey"};
            } else {
                // Check failure. A cross.
                return {dotColor:"red",text:"\uf00d",textColor:"white",circleColor:"grey"};
            }
        }
        // Unknown status. An exclamation mark warning.
        return {dotColor:"orange",text:"\uf12a",textColor:"black",circleColor:"grey"};
    };
    var calcIndicatorOpacity = function(d,i,historyCount){
        if(historyCount == maxHistoryItems && i == maxHistoryItems - 1){
            var proportion = calcIndicatorProportion(d);
            var opacity = 1.0 - proportion;
            //console.log("Fill opacity",proportion,opacity);
            return opacity;
        }
        return 1.0;
    };

    // console.log("checks: ",checks);
    checks.each(function (check, checkIndex) {
        var thisCheck = this;
        // console.log("each check", check, checkIndex, thisCheck);
        var checkData = _.flatMap(check[1], function (server) {
            return server[1];
        });
        // console.log("built checkData",checkData);
        var rings = d3.select(thisCheck).selectAll(".ring")
            .data(checkData)
            .enter()
            .append("circle")
            .attr("cx", function (d, checkIndex) {
                return calcRingCenterX(d, checkIndex);
            })
            .attr("cy", function (d, checkIndex) {
                return calcRingCenterY(d, checkIndex);
            })
            .attr("r", function (d) {
                return calcRingRadius(d);
            })
            .attr("stroke", "grey")
            .attr("fill", "none")
            .attr("stroke-width", 3);

        var indicators = d3.select(thisCheck).selectAll(".ringIndicator")
            .data(checkData)
            .enter();
        indicators.each(function(d,i){
            if ("history" in d){
                var history = _.concat([d],d.history);
                var thisIndicatorD = this;
                var historyCount = _.size(history);
                var historiesPositionX = calcRingCenterX(d,0) - dotRadius;
                var historiesPositionY = calcRingCenterY(d,0) - dotRadius - ringRadius;

                var historiesSvg = indicators.append("g");
                historiesSvg.attr("class","historiesContainer")
                    .attr("transform",function(hd,hi){
                      return sprintf("translate(%d,%d)",historiesPositionX,historiesPositionY);
                    })
                    .attr("w",historiesWidth)
                    .attr("h",historiesHeight)
                    .attr("x",historiesPositionX)
                    .attr("y",historiesPositionY);
/*
                historiesSvg.append("rect")
                    .attr("class","historiesBox")
                    .attr("w",historiesWidth)
                    .attr("h",historiesHeight)
                    .attr("x",0)
                    .attr("y",0)
                    .attr("fill","yellow");
*/
                var histories = d3.select(thisIndicatorD).selectAll(".historyItem")
                    .data(history)
                    .enter();
                histories.each(function(hd,currentHistoryIndex){
                    var historyAttributes = calcIndicatorAttributes(hd,true);

                    var historySvg = historiesSvg.append("g");
                    historySvg.attr("class","historyContainer")
                        .attr("transform", function() {
                            var offset = historyContainerHeight * calcIndicatorProportion(d);
                            return sprintf("translate(%d,%d)",
                                historyContainerPositionX,
                                historyContainerPositionY + (historyContainerHeight * currentHistoryIndex) + offset);
                        })
                        .attr("w",historyContainerWidth)
                        .attr("h",historyContainerHeight)
                        .attr("x",historyContainerPositionX)
                        .attr("y",function(){
                            return historyContainerPositionY + (historyContainerHeight * currentHistoryIndex);
                        })
                    historySvg.append("circle")
                        .attr("class","historyIndicator")
                        .attr("cy",dotRadius + 1)
                        .attr("cx",dotRadius + 1)
                        .attr("r",dotRadius)
                        .attr("stroke",historyAttributes.circleColor)
                        .attr("stroke-opacity",function(){
                            return calcIndicatorOpacity(d,currentHistoryIndex,historyCount);
                        })
                        .attr("fill",function(){
                            return historyAttributes.dotColor;
                        })
                        .attr("fill-opacity",function(){
                            return calcIndicatorOpacity(d,currentHistoryIndex,historyCount);
                        });
                    historySvg.append("text")
                        .attr("class","historyText")
                        .text(function() {
                            return historyAttributes.text;
                        })
                        .attr("x",(historyContainerWidth / 2) + historyTextOffsetX)
                        .attr("y",(historyContainerHeight / 2) + historyTextOffsetY)
                        .attr("text-anchor", "middle")
                        .attr("fill",function(){
                            return historyAttributes.textColor;
                        })
                        .attr("fill-opacity",function(){
                            return calcIndicatorOpacity(d,currentHistoryIndex,historyCount);
                        });
                    historySvg.append("rect")
                        .attr("class","historyDurationBackground")
                        .attr("x",(historyContainerWidth / 2) + historyDurationOffsetX - 2)
                        .attr("y",(historyContainerHeight / 2) - 8)
                        .attr("rx",5)
                        .attr("ry",5)
                        .attr("width",50)
                        .attr("height",15)
                        .attr("fill","url(#fadeRight)")
                        .attr("fill-opacity",function(){
                            return calcIndicatorOpacity(d,currentHistoryIndex,historyCount);
                        });
                    historySvg.append("text")
                        .attr("class","historyDuration")
                        .text(function() {
                            if (hd.duration !== undefined){
                              return sprintf("%d ms",hd.duration);
                            }
                        })
                        .attr("x",(historyContainerWidth / 2) + historyDurationOffsetX)
                        .attr("y",(historyContainerHeight / 2) + historyTextOffsetY)
                        .attr("text-anchor", "left")
                        .attr("fill",function(){
                            return "black";
                        })
                        .attr("fill-opacity",function(){
                            return calcIndicatorOpacity(d,currentHistoryIndex,historyCount);
                        });
                });
            }
        });

        var calcOrbiterX = function(d,i){
            if ("lastCheck" in d && "period" in d && "severity" in d){
                var center = calcRingCenterX(d, i);
                var radius = calcRingRadius(d);
                var theta = (2 * Math.PI * calcIndicatorProportion(d)) - (Math.PI / 2);
                return center + radius * Math.cos(theta) - (orbitingContainerWidth / 2);
            } else {
                return orbitingContainerWidth;
            }
        };
        var calcOrbiterY = function(d,i){
            if ("lastCheck" in d && "period" in d && "severity" in d){
                var center = calcRingCenterY(d, i);
                var radius = calcRingRadius(d);
                var theta = (2 * Math.PI * calcIndicatorProportion(d)) - (Math.PI / 2);
                return center + radius * Math.sin(theta) - (orbitingContainerHeight / 2);
            } else {
                return orbitingContainerHeight;
            }
        };
        var orbitingContainer = indicators.append("g")
            .attr("class","orbitingContainer")
            .attr("transform",function(d,i){
              return sprintf("translate(%s,%s)",calcOrbiterX(d,i),calcOrbiterY(d,i));
            })
            .attr("w",orbitingContainerWidth)
            .attr("h",orbitingContainerHeight)
            .attr("x",calcOrbiterX)
            .attr("y",calcOrbiterY);
        orbitingContainer.append("circle")
            .attr("class","orbitingIndicator")
            .attr("r",dotRadius)
            .attr("cx",orbitingContainerWidth / 2)
            .attr("cy",orbitingContainerHeight / 2)
            .attr("stroke",function(d){
                return calcIndicatorAttributes(d).circleColor;
            })
            .attr("fill",function(d){
                return calcIndicatorAttributes(d).dotColor;
            })
            .attr("stroke-width",1);
        orbitingContainer.append("text")
            .attr("class","orbitingText")
            .text(function(d) {
                return calcIndicatorAttributes(d).text;
            })
            .attr("x",(orbitingContainerWidth / 2) + indicatorTextOffsetX)
            .attr("y",(orbitingContainerHeight / 2) + indicatorTextOffsetY)
            .attr("text-anchor", "middle")
            .attr("fill",function(d,i){
                return calcIndicatorAttributes(d).textColor;
            });
        });
	};
	var renderHtml = (function() {
		
    var templates = {};
    $(function(){
        var templateRoot = $("#templateContainer");
        templates = {
            "service":templateRoot.find(".serviceTemplate").clone(),
            "server":templateRoot.find(".serverTemplate").clone(),
            "check":templateRoot.find(".checkTemplate").clone(),
            "checkDetail":templateRoot.find(".checkDetailTemplate").clone(),
            "checkDetailContainer":templateRoot.find(".checkDetailContainerTemplate").clone(),
            "collapser":templateRoot.find(".collapserTemplate").clone()
        };
        templateRoot.remove();
    });

    var toggledInClass = "toggledIn";
    var toggledOutClass = "toggledOut";
    var hideableHiddenClass = "hideableHidden";

    function getCollapser(char) {
        var collapser = templates["collapser"].clone();
        collapser.find(".collapserState").text(char);
        return collapser;
    }
    var getCollapserClosed = function(){return getCollapser("\uf055");};
    var getCollapserOpen = function(){return getCollapser("\uf056");};
    function setupCollapser(containerNode, containerName, collapserSelector, hideableSelector, expandCommand, collapseCommand, defaultExpanded) {
        var collapser = containerNode.find(collapserSelector);
        collapser.html(getCollapserClosed());

        var hideable = containerNode.find(hideableSelector);
        var expanded = false;

        var expand = function () {
            collapser.addClass(toggledInClass).removeClass(toggledOutClass);
            hideable.removeClass(hideableHiddenClass);
            collapser.html(getCollapserOpen());
            expanded = true;
            pluginSystem.fireCommand('layoutChanged', expandCommand);
        };
        var collapse = function () {
            collapser.addClass(toggledOutClass).removeClass(toggledInClass);
            hideable.addClass(hideableHiddenClass);
            collapser.html(getCollapserClosed());
            expanded = false;
            pluginSystem.fireCommand('layoutChanged', collapseCommand);
        };
        collapser.on('click', function () {
            if (expanded) {
                collapse();
            } else {
                expand();
            }
        });

        collapse();
        if (_.find(defaultExpanded, function (item) {
                return item == containerName;
            }) != undefined) {
            expand();
        }
    }

    var generateServiceId = function(serviceName){return safetyId("service_"+serviceName);};
    var createServiceElem = function(servers,serviceName,serviceLabel,withElem) {
        var serviceNode = templates["service"].clone();
        serviceNode.attr("id",generateServiceId(serviceName));
        serviceNode.find(".serviceCollapser").html(getCollapserOpen());
        serviceNode.find(".serviceLabel").text(serviceLabel);
        // setupCollapser(serviceNode, serviceName, ".serviceCollapser", ".serviceHideable", "core.expandService", "core.collapseService", defaultExpandedServices);
        return withElem(serviceNode,serviceName,servers);
    };
    var updateServiceElem = function(serviceNode,serviceName,serviceLabel,servers){
        return serviceNode;
    };

    var generateServerId = function(serverName){return safetyId("server_"+serverName);};
    var updateServerElem = function(serverNode,serverName,serverLabel,server){
        return serverNode;
    };
    var createServerElem = function(checks,serverName,serverLabel,withElem) {
        var serverNode = templates["server"].clone();
        serverNode.attr("id",generateServerId(serverName));
        serverNode.find(".serverLabel").text(serverLabel);
        // setupCollapser(serverNode, serverName, ".serverCollapser", ".serverHideable", "core.expandServer", "core.collapseServer", defaultExpandedServers);
        return withElem(serverNode,serverName,serverLabel,checks);
    };

    var calcCheckSeverity = function(severity) {
        switch(severity) {
            case "IMPACT": return {icon:"\uf06d",text:"Customer Impact"}; // fire
            case "ISSUE": return {icon:"\uf024",text:"Technical Issue"};  // flag
            case "ALERT": return {icon:"\uf21e",text:"Technical Alert"};  // heartbeat
        }
        // Unknown severity. A question mark.
        return {icon:"\uf059",text:"Unknown"};
    };

    var formatTimespan = function(timespan) {
      return TimeSpanFormatter.formatTimeSpan(timespan);
    }

    var generateCheckId = function(check){return safetyId("check_"+check.id);};
    var createCheckElem = function(check,withElem){
        var checkNode = templates["check"].clone();
        checkNode.attr("id",generateCheckId(check));
        checkNode.find(".checkName").text(check.name);
        var checkLabel = checkNode.find(".checkLabel");
        checkLabel.text(check.label);
        checkNode.find(".checkServiceName").text(check.serviceName);
        checkNode.find(".checkServiceLabel").text(check.serviceLabel);
        checkNode.find(".checkServerName").text(check.serverName);
        checkNode.find(".checkServerLabel").text(check.serverLabel);
        checkNode.find(".checkSeverity").text(check.severity);
        var checkSeverity = calcCheckSeverity(check.severity);
        checkNode.find(".checkSeverityIcon").text(checkSeverity.icon);
        checkNode.find(".checkSeverityContainer").find(".tooltiptext").text(checkSeverity.text);
        checkNode.find(".checkMode").text(check.mode);
        checkNode.find(".checkFrequency").text(formatTimespan(check.period));
        setupCollapser(checkNode, check.name, ".checkCollapser", ".checkHideable", "core.expandCheck", "core.collapseCheck", defaultExpandedChecks);
        return withElem(checkNode,check);
    };
    var updateCheckElem = function(checkNode,check){
        checkNode.find(".lastSuccess").text(new Date(check.lastUp));
        checkNode.find(".lastCheck").text(new Date(check.lastCheck));
        checkNode.find(".checkWhy").text(check.why);
        checkNode.find(".checkDetail").text(check.detail);
        return checkNode;
    };

    var safetyId = function(input){
        return _.replace(input," ","_");
    };
    var render = function(rootSelectorString,checkStructure) {
        var serviceNameToLabel = {};
        var serverNameToLabel = {};

        _.forEach(checkStructure, function(check){
            if(!serviceNameToLabel[check.serviceName]) serviceNameToLabel[check.serviceName] = check.serviceLabel;
            if(!serverNameToLabel[check.serverName]) serverNameToLabel[check.serverName] = check.serverLabel;
        });

        var structure = _.mapValues(
            _.groupBy(checkStructure,
                      function (check) {
                          return check.serviceName;
                      }),
            function (service) {
                return _.groupBy(service, function (check) {
                    return check.serverName;
                });
            });

        var rootElem = $(rootSelectorString);
        var existingDomElements = rootElem.find(".checkSummary");
        _.forEach(existingDomElements,function(domElement) {
            var domId = $(domElement).attr("id").split("_")[1];
            var matchingElem = _.find(checkStructure,function(ce){return ce.id == domId});
            if (matchingElem === undefined){
                $(domElement).remove();
            }
        });

        checkStructure = _.sortBy(checkStructure,['status','serviceLabel','serverLabel','label']);

        _.forEach(checkStructure,function(check,checkName){
           var checkRoot = rootElem.find("#"+generateCheckId(check));
           if (checkRoot[0] === undefined){
               checkRoot = createCheckElem(check,updateCheckElem);
               rootElem.append(checkRoot);
           } else {
               if ("dirty" in check && check.dirty === true) {
                   updateCheckElem(checkRoot, check);
                   delete check.dirty;
               }
           }
           checkRoot.find(".checkSvg").empty();
           var singleCheckStructure = _.filter(checkStructure, function(o){
                return o.id === check.id;
            });
           renderCheckSvg(checkRoot.find(".checkSvg"), singleCheckStructure);
        });

        var passing = _.size(_.filter(checkStructure,function(check){return check.status;}));
        $("#passCount").text(passing);
        $("#passDesc").text("Pass"+(passing > 1 ? "es" : ""));
        var failing = _.size(_.filter(checkStructure,function(check){return !check.status;}));
        if(failing > 0) {
          $("#failCount").text(failing);
          $("#failDesc").text("Fail"+(failing > 1 ? "s" : ""));
        } else {
          $("#checksFailing").attr("style","display:none;");
        }
    };

    return function(rootSelectorString,checkStructure){
        render(rootSelectorString,checkStructure);
    };
	})();



});
