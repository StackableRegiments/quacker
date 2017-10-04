var renderHtml = (function(rootSelectorString,jsonStructure) {
    var templates = {};
    $(function(){
        var templateRoot = $("#templateContainer");
        console.log("found templateRoot",templateRoot);
        templates = {
            "service":templateRoot.find(".serviceTemplate").clone(),
            "server":templateRoot.find(".serverTemplate").clone(),
            "check":templateRoot.find(".checkTemplate").clone(),
            "checkDetail":templateRoot.find(".checkDetailTemplate").clone(),
            "checkDetailContainer":templateRoot.find(".checkDetailContainerTemplate").clone(),
            "collapser":templateRoot.find(".collapserTemplate").clone()
        };
        templateRoot.remove();
        console.log("found templates",templates);
    });

    var toggledInClass = "toggledIn";
    var toggledOutClass = "toggledOut";
    var hideableHiddenClass = "hideableHidden";

    function getCollapser(char) {
        var collapser = templates["collapser"].clone();
        collapser.find(".collapserState").text(char);
        return collapser;
    }
    var getCollapserClosed = function(){return getCollapser("+");};
    var getCollapserOpen = function(){return getCollapser("-");};
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
    var createServiceElem = function(servers,serviceName,withElem) {
        var serviceNode = templates["service"].clone();
        serviceNode.attr("id",generateServiceId(serviceName));
        serviceNode.find(".serviceCollapser").html(getCollapserOpen());
        serviceNode.find(".serviceName").text(serviceName);
        setupCollapser(serviceNode, serviceName, ".serviceCollapser", ".serviceHideable", "core.expandService", "core.collapseService", defaultExpandedServices);
        return withElem(serviceNode,serviceName,servers);
    };
    var updateServiceElem = function(elem,serviceName,servers){
        return elem;
    };

    var generateServerId = function(serverName){return safetyId("server_"+serverName);};
    var updateServerElem = function(elem,serverName,server){
        return elem;
    };
    var createServerElem = function(checks,serverName,withElem) {
        var serverNode = templates["server"].clone();
        serverNode.attr("id",generateServerId(serverName));
        serverNode.find(".serverName").text(serverName);
        setupCollapser(serverNode, serverName, ".serverCollapser", ".serverHideable", "core.expandServer", "core.collapseServer", defaultExpandedServers);
        return withElem(serverNode,serverName,checks);
    };

    var generateCheckId = function(check){return safetyId("check_"+check.id);};
    var createCheckElem = function(check,withElem){
        var checkNode = templates["check"].clone();
        checkNode.attr("id",generateCheckId(check));
        checkNode.find(".checkName").text(check.name);
        checkNode.find(".checkLabel").text(check.label);
        checkNode.find(".checkService").text(check.service);
        checkNode.find(".checkServer").text(check.server);
        checkNode.find(".checkSeverity").text(check.severity);
        checkNode.find(".checkMode").text(check.mode);
        setupCollapser(checkNode, check.name, ".checkCollapser", ".checkHideable", "core.expandCheck", "core.collapseCheck", defaultExpandedChecks);
/*
// Pinger

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
*/

/*
//Information
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
*/

/*
//Error
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
*/

        return withElem(checkNode,check);
    };
    var updateCheckElem = function(elem,check){
      elem.find(".statusCode").text(check.status ? "Y" : "N");
      elem.find(".lastSuccess").text(new Date(check.lastUp));
      elem.find(".lastCheck").text(new Date(check.lastCheck));
      elem.find(".why").text(check.why);
      elem.find(".detail").text(check.detail);
      return elem;
    };

    var safetyId = function(input){
        return _.replace(input," ","_");
    };
    var render = function(rootSelectorString,jsonStructure) {
        var structure = _.mapValues(_.groupBy(jsonStructure, function (check) {
            return check.service;
        }), function (service) {
            return _.groupBy(service, function (check) {
                return check.server;
            });
        });
        //console.log("structure",structure);
        var rootElem = $(rootSelectorString);
        _.forEach(structure,function(servers,serviceName) {
            var serviceRoot = rootElem.find("#"+generateServiceId(serviceName));
            if (serviceRoot[0] === undefined){
                serviceRoot = createServiceElem(servers,serviceName,updateServiceElem);
                // console.log("creating rootElem",serviceRoot,rootElem);
                rootElem.append(serviceRoot);
            } else {
                updateServiceElem(serviceRoot,serviceName,servers);
                // console.log("updating rootElem",serviceRoot,rootElem);
            }
            var serverContainer = serviceRoot.find(".servers");
            _.forEach(servers,function(checks,serverName){
                var serverRoot = serverContainer.find("#"+generateServerId(serverName));
                if (serverRoot[0] === undefined){
                    serverRoot = createServerElem(checks,serverName,updateServerElem);
                    serverContainer.append(serverRoot)
                } else {
                    updateServerElem(serverRoot,serverName,checks);
                }
                var checksContainer = serverRoot.find(".checks");
                _.forEach(checks,function(check){
                   var checkRoot = checksContainer.find("#"+generateCheckId(check));
                   if (checkRoot[0] === undefined){
                       checkRoot = createCheckElem(check,updateCheckElem);
                       checksContainer.append(checkRoot);
                   } else {
                       updateCheckElem(checkRoot,check);
                   }
                });
            });
        });
    };

    return function(rootSelectorString,jsonStructure){
        render(rootSelectorString,jsonStructure);
    };
})();
