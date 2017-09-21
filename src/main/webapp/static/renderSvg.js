var renderSvg = function (rootId) {
    var isHidden = function (elem) {
        return !elem.is(":visible") || elem.is(":hidden") || elem.css("display") === "none";
    };
    var serviceLabelHeight = 20;
    var serverLabelHeight = 15;
    var checkLabelHeight = 10;

    var svg = d3.select(rootId).append("svg");
    var data = _.toPairs(_.mapValues(_.groupBy(jsonStructure, function (item) {
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
        console.log("calculating offset:", serverIndex, checkIndex, result);
        return result;
    }

    var constructIdentity = function (inString) {
        return _.replace(inString, " ", "_");
    };
    // console.log("serviceData",data);
    var dGroup = {
        width: 200,
        height: 250
    };
    var services = svg
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
    var serviceLabels = services
        .append("text")
        .attr("class", "serviceLabel")
        .attr("text-anchor", "middle")
        .attr("x", dGroup.width / 2)
        .attr("y", dGroup.height)
        .text(function (d, i) {
            //console.log("Service label:",d,i);
            return d[0];
        })
        .on("click", function (d, i) {
            var selectorString = "#" + constructIdentity("service_" + d[0]) + " .server";
            var selector = $(selectorString);
            console.log("showing:", selectorString, selector, isHidden(selector));
            if (isHidden(selector)) {
                selector.show();
            } else {
                selector.hide();
            }
        });
    services.each(function (service, serviceIndex) {
        var thisService = this;
        console.log("each service", service, serviceIndex, thisService);
        var servers = d3.select(thisService).selectAll(".server")
            .data(service[1])
            .enter().append("g")
            .attr("class", "server")
            .attr("id", function (d, i) {
                return constructIdentity("server_" + d[0]);
            });
        var serverLabels = servers
            .append("text")
            .attr("class", "serverLabel")
            .attr("x", 60)
            .attr("y", function (d, i, coll, e) {
                var above = calculateVerticalOffsetWithinService(serviceIndex, i);
                console.log("serverLabels", d, i, coll, above);
                return dGroup.height + above;
            })
            .text(function (d) {
                return d[0];
            })
            .on("click", function (d, i) {
                var selectorString = "#" + constructIdentity("server_" + d[0]) + " .check";
                var selector = $(selectorString);
                console.log("showing:", selectorString, selector, isHidden(selector));
                if (isHidden(selector)) {
                    selector.show();
                } else {
                    selector.hide();
                }
            });
        servers.each(function (server, serverIndex) {
            var thisServer = this;
            console.log("each server", server, serverIndex, thisServer);
            var checks = d3.select(thisServer)
                .selectAll(".check")
                .data(server[1])
                .enter().append("g")
                .attr("class", "check")
                .attr("id", function (d, i) {
                    return constructIdentity("check_" + d.id);
                });
            var checkLabels = checks
                .append("text")
                .attr("class", "checkLabel")
                .attr("x", 90)
                .attr("y", function (sd, si, coll) {
                    var above = calculateVerticalOffsetWithinService(serviceIndex, serverIndex, si);
                    console.log("checkLabels", sd, si, coll, above);
                    return dGroup.height + above;
                })
                .text(function (d, i) {
                    return d.label;
                })
                .on("mouseover", function (d, i) {
                    // console.log('mouseOverElem',this,d,i);
                });
        });
    });
    var rings = services
        .append("g")
        .data([30, 60, 90])
        .append("circle")
        .attr("cx", dGroup.width / 2)
        .attr("cy", dGroup.height / 2)
        .attr("r", function (d) {
            return d
        })
        .attr("stroke", "black")
        .attr("fill", "none")
        .attr("stroke-width", 2);
};
