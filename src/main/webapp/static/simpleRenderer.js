pluginSystem.suspendCommand('dataChanged');
pluginSystem.suspendCommand('layoutChanged');
pluginSystem.suspendCommand('createCheck');
pluginSystem.suspendCommand('removeCheck');
$(function(){

	var templates = {};
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

	pluginSystem.subscribe('dataChanged','flexibleRenderer.internalUpdateCheck',function(obj){return internalUpdateCheck(obj);});
	pluginSystem.subscribe('createCheck','flexibleRenderer.internalCreateCheck',function(obj){return internalCreateCheck(obj);});
	pluginSystem.subscribe('removeCheck','flexibleRenderer.internalRemoveCheck',function(obj){return internalRemoveCheck(obj);});

	pluginSystem.resumeCommand('dataChanged');
	pluginSystem.resumeCommand('layoutChanged');
	pluginSystem.resumeCommand('createCheck');
	pluginSystem.resumeCommand('removeCheck');

	var rootElem = $("#dashboardServerContainer");
	function internalUpdateCheck(check,targetNode){
			//console.log('Simple.updateCheck',check);
			var checkName = check.name;
			var checkRoot = rootElem.find("#"+generateCheckId(check));
			updateCheckElem(checkRoot, check);
			updateCounts();
	}
	function internalCreateCheck(newCheck){
		//console.log('Simple.createCheck',newCheck);
		var serviceElem = rootElem.find("#"+generateServiceId(newCheck.serviceName));
		if (serviceElem[0] === undefined){
			serviceElem = createServiceElem([],newCheck.serviceName,newCheck.serviceLabel,function(e){return e; });
			rootElem.append(serviceElem);
		}
		var serversRoot = serviceElem.find(".servers");
		var serverElem = serversRoot.find("#"+generateServerId(newCheck.serverName));
		if (serverElem[0] === undefined){
			serverElem = createServerElem([],newCheck.serverName,newCheck.serverName,function(e){ return e; });
			serversRoot.append(serverElem);
		}
		var checksRoot = serverElem.find(".checks");
		var checkName = newCheck.name;
		var checkRoot = checksRoot.find("#"+generateCheckId(newCheck));
		if (checkRoot[0] === undefined){
				 checkRoot = createCheckElem(newCheck,updateCheckElem);
				 checksRoot.append(checkRoot);
		} else {
			 if ("dirty" in newCheck && newCheck.dirty === true) {
					 updateCheckElem(checkRoot, newCheck);
					 delete newCheck.dirty;
			 }
		}
		updateCounts();
	}
	function internalRemoveCheck(checkId){
		//console.log('Simple.removeCheck',checkId);
		rootElem.find("#"+generateCheckId(check)).remove();
		updateCounts();
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
	function setupCollapser(containerNode, elemType, containerName, collapserSelector, hideableSelector, expandCommand, collapseCommand, defaultExpanded, hideExpander) {
			var collapser;
			var implicitCollapse = true;
			if (collapserSelector === undefined){
				collapser = containerNode;
			} else {
				collapser = containerNode.find(collapserSelector);
			}
			if (!hideExpander){
				collapser.html(getCollapserClosed());
			} else {
				collapser.hide();
			}

			var hideable = containerNode.find(hideableSelector);
			var expanded = false;

			var toggleFunc = function (e) {
					e.preventDefault();
					e.stopPropagation();
					//console.log('toggle',e);
					if (expanded) {
							collapse();
					} else {
							expand();
					}
					return false;
			};

			var expand = function () {
					collapser.addClass(toggledInClass).removeClass(toggledOutClass);
					hideable.removeClass(hideableHiddenClass);
					collapser.html(getCollapserOpen()).show();
					expanded = true;
					pluginSystem.fireCommand('layoutChanged', expandCommand);
					containerNode.unbind('click');
					collapser.unbind('click').on('click', toggleFunc);
			};
			var collapse = function () {
					collapser.addClass(toggledOutClass).removeClass(toggledInClass);
					hideable.addClass(hideableHiddenClass);
					if (!hideExpander){
						collapser.html(getCollapserClosed());
					} else {
						collapser.hide();
					}
					expanded = false;
					pluginSystem.fireCommand('layoutChanged', collapseCommand);
					collapser.unbind('click').on('click', toggleFunc);
					if (implicitCollapse){
						containerNode.unbind('click').on('click',toggleFunc);
					}
			};
			collapse();
			if (_.startsWith(elemType,'server') || _.startsWith(elemType,'service') || _.find(defaultExpanded, function (item) {
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
			setupCollapser(serviceNode, 'service', serviceName, ".serviceCollapser", ".serviceHideable", "core.expandService", "core.collapseService", defaultExpandedServices);
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
			setupCollapser(serverNode, 'server', serverName, ".serverCollapser", ".serverHideable", "core.expandServer", "core.collapseServer", defaultExpandedServers);
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

	var generateCheckId = function(check){return 'id' in check ? safetyId("check_"+check.id) : safetyId("check_"+check);};
	var createCheckElem = function(check,withElem){
			var checkNode = templates["check"].clone();
			checkNode.attr("id",generateCheckId(check));
			checkNode.find(".checkName").text(check.name);
			var checkLabel = checkNode.find(".checkLabel");
			checkLabel.text(check.label);
			var header = checkNode.find(".checkHeader");
			header.find(".checkLabel").remove();
			var line1 = checkNode.find(".summaryLine1");
			line1.find(".checkServiceContainer").remove();
			line1.find(".checkServerContainer").remove();
			var line2 = checkNode.find(".summaryLine2");
			checkNode.find(".checkServiceName").text(check.serviceName);
			checkNode.find(".checkServiceLabel").text(check.serviceLabel);
			checkNode.find(".checkServerName").text(check.serverName);
			checkNode.find(".checkServerLabel").text(check.serverLabel);
			checkNode.find(".checkSeverity").text(check.severity);
			var checkSeverity = calcCheckSeverity(check.severity);
			//checkNode.find(".checkSeverityIcon").text(checkSeverity.icon);
			checkNode.find(".checkSeverityIcon").remove();
			var cs = checkNode.find(".checkStatus");
			if ('status' in check){
				if (check.status){
					cs.text('Y');
					cs.addClass('checkOk').removeClass('checkError').removeClass('checkUnknown');
				} else {
					cs.text('N');
					cs.removeClass('checkOk').addClass('checkError').removeClass('checkUnknown');
				}
			} else {
				cs.text('?');
				cs.removeClass('checkOk').removeClass('checkError').addClass('checkUnknown');
			}
			//checkNode.find(".checkSeverityContainer").find(".tooltiptext").text(checkSeverity.text);
			//checkNode.find(".checkSeverityContainer").find(".tooltiptext").text(check.name);
			checkNode.find(".checkMode").text(check.mode);
			checkNode.find(".checkFrequency").text(formatTimespan(check.period));
			setupCollapser(checkNode, 'check', check.name, ".checkCollapser", ".checkHideable", "core.expandCheck", "core.collapseCheck", defaultExpandedChecks,true);
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
			return input.replaceAll(" ","_").replaceAll(".","_");
	};

	function updateCounts(){
		var passing = _.size(_.filter(jsonStructure,function(check){return check.status;}));
		$("#passCount").text(passing);
		$("#passDesc").text("Pass"+(passing > 1 ? "es" : ""));
		var failing = _.size(_.filter(jsonStructure,function(check){return !check.status;}));
		if(failing > 0) {
			$("#failCount").text(failing);
			$("#failDesc").text("Fail"+(failing > 1 ? "s" : ""));
		} else {
			$("#checksFailing").attr("style","display:none;");
		}
	};
});
