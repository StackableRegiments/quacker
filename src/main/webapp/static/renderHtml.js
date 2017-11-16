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

    var calcCheckSeverityIcon = function(severity) {
        switch(severity) {
/*            case "IMPACT": return "\uf2c7"; // thermometer full
            case "ISSUE": return "\uf2c9";  // thermometer half
            case "ALERT": return "\uf2cb";  // thermometer empty    */
            case "IMPACT": return "\uf06d"; // fire
            case "ISSUE": return "\uf024";  // flag
            case "ALERT": return "\uf21e";  // heartbeat
        }
        // Unknown severity. A question mark.
        return "\uf059";
    };

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
        checkNode.find(".checkSeverityIcon").text(calcCheckSeverityIcon(check.severity));
        checkNode.find(".checkMode").text(check.mode);
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
    };

    return function(rootSelectorString,checkStructure){
        render(rootSelectorString,checkStructure);
    };
})();
