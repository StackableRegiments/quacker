var dGroup = {
    width: 300,
    height: 250
};

var renderCheckSvg = function(checkElem, allChecks) {
    var constructIdentity = function (inString) {
        return _.replace(inString, " ", "_");
    };
    var svg = d3.select(checkElem[0]).append("svg");

    var data = _.toPairs(_.mapValues(_.groupBy(allChecks, function (item) {
        return item.service;
    }), function (v, k) {
        return _.toPairs(_.groupBy(v, function (vi, vk) {
            return vi.server;
        }));
    }));

    var checks = svg.attr("viewBox",sprintf("0 0 %s %s", dGroup.width/* * serviceCount*/, dGroup.height))
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

    var severityRings = {
        "impact":30,
        "issue":30,
        "alert":30
    };
    var calcRadius = function(check){
        return severityRings[check.severity.toLowerCase()];
    };
    var calcX = function(check, checkIndex) {
        return (dGroup.width / 2) * checkIndex + 40;
    };
    var calcY = function(check, checkIndex) {
        return dGroup.height / 2;
    };
    var calcProportion = function(d) {
        if("lastCheck" in d && "period" in d) {
            var now = new Date().getTime();
            return (now - d.lastCheck) / d.period;
        } else {
            return 0;
        }
    }
    var calcIndicatorColor = function(d) {
        // console.log("Calcing color",d,d.lastCheck,d.status,d.label);
        if("lastCheck" in d && "period" in d) {
            var nextCheck = (d.lastCheck + d.period);
            if (d.lastCheck == 0 || new Date().getTime() > nextCheck) {
                return "grey";
            }
        }
        if("status" in d) {
            if (d.status == true) {
                return "green";
            } else {
                return "red";
            }
        }
        return "orange";
    }
    // console.log("checks: ",checks);
    checks.each(function (check, checkIndex) {
        var thisCheck = this;
//        console.log("each check", check, checkIndex, thisCheck);
        var checkData = _.flatMap(check[1], function (server) {
            return server[1];
        });
        //console.log("built checkData",checkData);
        var rings = d3.select(thisCheck).selectAll(".ring")
            .data(checkData)
            .enter()
            .append("circle")
            .attr("cx", function (d, checkIndex) {
                return calcX(d, checkIndex);
            })
            .attr("cy", function (d, checkIndex) {
                return calcY(d, checkIndex);
            })
            .attr("r", function (d) {
                //console.log("r",d)
                return calcRadius(d);
            })
            .attr("stroke", "black")
            .attr("fill", "none")
            .attr("stroke-width", 2);
        var text = d3.select(thisCheck).selectAll(".text")
            .data(checkData)
            .enter()
            .append("text")
            .text(function(d) {
                return d.label;
            })
            .attr("text-anchor", "middle")
            .attr("x", function (d, checkIndex) {
                return calcX(d, checkIndex);
            })
            .attr("y", function (d, checkIndex) {
                return calcY(d, checkIndex);
            });

        var indicators = d3.select(thisCheck).selectAll(".ringIndicator")
            .data(checkData)
            .enter()
            .append("circle")
            .attr("cx",function(d,i){
                if ("lastCheck" in d && "period" in d && "severity" in d){
                    var center = calcX(d, i);
                    var radius = calcRadius(d);
                    var theta = (2 * Math.PI * calcProportion(d)) - (Math.PI / 2);
                    return center + radius * Math.cos(theta);
                } else {
                    return 25;
                }
            })
            .attr("cy",function(d,i){
                if ("lastCheck" in d && "period" in d && "severity" in d){
                    var center = calcY(d, i);
                    var radius = calcRadius(d);
                    var theta = (2 * Math.PI * calcProportion(d)) - (Math.PI / 2);
                    return center + radius * Math.sin(theta);
                } else {
                    return 25;
                }
            })
            .attr("class","checkIndicator")
            .attr("r",5)
            .attr("stroke","grey")
            .attr("fill",function(d){
                return calcIndicatorColor(d);
            })
            .attr("stroke-width",1);
    });
};
/*
var renderSvgCircles = function (allChecks, serviceName) {
    // var isHidden = function (elem) {
    //     return !elem.is(":visible") || elem.is(":hidden") || elem.css("display") === "none";
    // };
    // var serviceLabelHeight = 20;
    // var serverLabelHeight = 15;
    // var checkLabelHeight = 10;

    var severityRings = {
        "impact":30,
        "issue":30,
        "alert":30
    };
    var calcRadius = function(check){
        return severityRings[check.severity.toLowerCase()];
    };
    var calcX = function(check, serviceIndex) {
        return (dGroup.width / 2) * serviceIndex;
    };
    var calcY = function(check, serviceIndex) {
        return dGroup.height / 2;
    };

    var rootElem = $("<div/>")[0];
    var svg = d3.select(rootElem).append("svg");
    var data = _.toPairs(_.mapValues(_.groupBy(allChecks, function (item) {
        return item.service;
    }), function (v, k) {
        return _.toPairs(_.groupBy(v, function (vi, vk) {
            return vi.server;
        }));
    }));
    var calculateVerticalOffsetWithinService = function (serviceIndex, serverIndex, checkIndex) {
        var serviceTup = _.find(data, function (serviceTup, svcInd) {
            return svcInd == serviceIndex;
        });
        var serviceName = serviceTup[0];
        var servers = serviceTup[1];
        var svcLabel = serviceLabelHeight;
        var result = svcLabel + _.sum(_.map(servers, function (srvTup, srvInd) {
            var serverName = srvTup[0];
            var checks = srvTup[1];
            var srvLabel = (serverIndex > srvInd) ? serverLabelHeight : 0;
            return srvLabel + _.sum(_.map(checks, function (check, chkInd) {
                if (serverIndex > srvInd) {
                    return checkLabelHeight;
                } else if (serverIndex == srvInd) {
                    if (checkIndex > chkInd) {
                        return checkLabelHeight;
                    } else if (checkIndex == chkInd) {
                        return checkLabelHeight;
                    } else {
                        return 0;
                    }
                } else {
                    return 0;
                }

            }));
        }));
        // console.log("calculating offset:", serverIndex, checkIndex, result);
        return result;
    };

    var constructIdentity = function (inString) {
        return _.replace(inString, " ", "_");
    };
    // console.log("serviceData",data);
    var dGroup = {
        width: 250,
        height: 250
    };
    var serviceCount = _.size(data);
    // console.log("data",data);
//    var services = svg.attr("viewBox",sprintf("0 0 %s %s", dGroup.width * serviceCount, dGroup.height))
    var services = svg.attr("viewBox",sprintf("0 0 %s %s", dGroup.width, dGroup.height))
        .selectAll(".service")
        .data(data)
        .enter().append("g")
        .attr("class", "service")
        .attr("id", function (d, i) {
            return constructIdentity("service_" + d[0]);
        })
        .attr("height", dGroup.height)
        .attr("width", dGroup.width)
        .attr("transform", function (d, i) {
            return sprintf("translate(%s,%s)", i * dGroup.width, 0);
        });

    services.each(function (service, serviceIndex) {
        var thisService = this;
        // console.log("each service", service, serviceIndex, thisService);
        var ringData = _.flatMap(service[1],function(server){ return server[1];});
        //console.log("built ringData",ringData);
        var rings = d3.select(thisService).selectAll(".ring")
            .data(ringData)
            .enter()
            .append("circle")
            .attr("cx", function(d, serviceIndex) {
                return calcX(d, serviceIndex);
            })
            .attr("cy", function(d, serviceIndex) {
                return calcY(d, serviceIndex);
            })
            .attr("r", function (d) {
                //console.log("r",d)
                return calcRadius(d);
            })
            .attr("stroke", "black")
            .attr("fill", "none")
            .attr("stroke-width", 2);

        function calcProportion(d) {
            if("lastCheck" in d && "period" in d) {
                var now = new Date().getTime();
                return (now - d.lastCheck) / d.period;
            } else {
                return 0;
            }
        }

        function calcIndicatorColor(d) {
            // console.log("Calcing color",d,d.lastCheck,d.status,d.label);
            if("lastCheck" in d && "period" in d) {
                var nextCheck = (d.lastCheck + d.period);
                if (d.lastCheck == 0 || new Date().getTime() > nextCheck) {
                    return "grey";
                }
            }
            if("status" in d) {
                if (d.status == true) {
                    return "green";
                } else {
                    return "red";
                }
            }
            return "orange";
        }

        var indicators = d3.select(thisService).selectAll(".ringIndicator")
            .data(ringData)
            .enter()
            .append("circle")
            .attr("cx",function(d,i){
                if ("lastCheck" in d && "period" in d && "severity" in d){
                    var center = calcX(d, i);
                    var radius = calcRadius(d);
                    var theta = (2 * Math.PI * calcProportion(d)) - (Math.PI / 2);
                    return center + radius * Math.cos(theta);
                } else {
                    return 25;
                }
            })
            .attr("cy",function(d,i){
                if ("lastCheck" in d && "period" in d && "severity" in d){
                    var center = calcY(d, i);
                    var radius = calcRadius(d);
                    var theta = (2 * Math.PI * calcProportion(d)) - (Math.PI / 2);
                    return center + radius * Math.sin(theta);
                } else {
                    return 25;
                }
            })
            .attr("class","checkIndicator")
            .attr("r",5)
            .attr("stroke","grey")
            .attr("fill",function(d){
                return calcIndicatorColor(d);
            })
            .attr("stroke-width",1);
        var servers = d3.select(thisService)
            .selectAll(".server")
            .data(service[1])
            .enter().append("g")
            .attr("class", "server")
            .attr("id", function (d, i) {
                return constructIdentity("server_" + d[0]);
            });

        servers.each(function (server, serverIndex) {
            var thisServer = this;
            // console.log("each server", server, serverIndex, thisServer);
            var checks = d3.select(thisServer)
                .selectAll(".check")
                .data(server[1])
                .enter().append("g")
                .attr("class", "check")
                .attr("id", function (d, i) {
                    return constructIdentity("check_" + d.id);
                });
        });
    });
    return rootElem;
};
*/