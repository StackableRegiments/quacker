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
/*            case "IMPACT": return "\uf2c7"; // thermometer full
            case "ISSUE": return "\uf2c9";  // thermometer half
            case "ALERT": return "\uf2cb";  // thermometer empty    */
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

//        console.log("Check structure",checkStructure);

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
        /*
        _.forEach(structure,function(servers,serviceName) {
            var serviceRoot = rootElem.find("#"+generateServiceId(serviceName));
            var serviceLabel = serviceNameToLabel[serviceName];
            if (serviceRoot[0] === undefined){
                serviceRoot = createServiceElem(servers,serviceName,serviceLabel,updateServiceElem);
                 rootElem.append(serviceRoot);
            } else {
                updateServiceElem(serviceRoot,serviceName,serviceLabel,servers);
            }
            var serverContainer = serviceRoot.find(".servers");
            _.forEach(servers,function(checks,serverName){
                var serverRoot = serverContainer.find("#"+generateServerId(serverName));
                var serverLabel = serverNameToLabel[serverName];
                if (serverRoot[0] === undefined){
                    serverRoot = createServerElem(checks,serverName,serverLabel,updateServerElem);
                    serverContainer.append(serverRoot)
                } else {
                    updateServerElem(serverRoot,serverName,serverLabel,checks);
                }
                var checksContainer = serverRoot.find(".checks");
                _.forEach(checks,function(check,checkName){
                   var checkRoot = checksContainer.find("#"+generateCheckId(check));
                   if (checkRoot[0] === undefined){
                       checkRoot = createCheckElem(check,updateCheckElem);
                       checksContainer.append(checkRoot);
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
            });
        });
        */
//        var checksContainer = rootElem.find(".checks");
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
        var failing = _.size(_.filter(checkStructure,function(check){return !check.status;}));
        if(failing > 0) {
          $("#failCount").text(failing);
        } else {
          $("#checksFailing").attr("style","display:none;");
        }
    };

    return function(rootSelectorString,checkStructure){
        render(rootSelectorString,checkStructure);
//        $(".serviceOuter").tile(3);
//        $(".t").tile(3);
//        console.log("Tiled");
//        paused = true;
    };
})();
