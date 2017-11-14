var renderCheckSvg = function(checkElem, allChecks) {
    var dGroup = {
        width: 300,
        height: 250
    };
    var ringRadius = 75;
    var orbitingContainerWidth = 25;
    var orbitingContainerHeight = 25;
    var dotRadius = 10;
    var indicatorTextOffsetY = 4;
    var historyWidth = 102;
    var historyHeight = 27;
    var historyPositionX = 25;
    var historyPositionY = dGroup.height / 2 - historyHeight / 2;
    var historyContainerWidth = 25;
    var historyContainerHeight = 25;

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

    var calcRingRadius = function(check){
        return ringRadius;
    };
    var calcRingX = function(check, checkIndex) {
        return ((dGroup.width / 2) * checkIndex) + calcRingRadius(check) + dotRadius;
    };
    var calcRingY = function(check, checkIndex) {
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
    var calcIndicatorDotColor = function(d,ignoreNextCheck) {
        // console.log("Calcing color",d,d.lastCheck,d.status,d.label);
        if("lastCheck" in d && "period" in d) {
            var nextCheck = (d.lastCheck + d.period);
            if (d.lastCheck == 0 || (!ignoreNextCheck && new Date().getTime() > nextCheck)) {
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
    };
    var calcIndicatorTextColor = function(d,ignoreNextCheck){
        // console.log("Calcing color",d,d.lastCheck,d.status,d.label);
        if("status" in d) {
            if (d.status == true) {
                return "white";
            } else {
                return "white";
            }
        }
        if("lastCheck" in d && "period" in d) {
            var nextCheck = (d.lastCheck + d.period);
            if (d.lastCheck == 0 || (!ignoreNextCheck && new Date().getTime() > nextCheck)) {
                return "black";
            }
        }
        return "orange";
    };
    var calcIndicatorText = function(d) {
        // console.log("Calcing text",d,d.lastCheck,d.status,d.label);
        if("lastCheck" in d && "period" in d) {
            var nextCheck = (d.lastCheck + d.period);
            if (d.lastCheck == 0 || new Date().getTime() > nextCheck) {
                return "?";
            }
        }
        if("status" in d) {
            if (d.status == true) {
                return "Y";
            } else {
                return "N";
            }
        }
        return "O";
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
                return calcRingX(d, checkIndex);
            })
            .attr("cy", function (d, checkIndex) {
                return calcRingY(d, checkIndex);
            })
            .attr("r", function (d) {
                //console.log("r",d)
                return calcRingRadius(d);
            })
            .attr("stroke", "black")
            .attr("fill", "none")
            .attr("stroke-width", 2);

        var indicators = d3.select(thisCheck).selectAll(".ringIndicator")
            .data(checkData)
            .enter();
        indicators.each(function(d,i){
            if ("history" in d && _.size(d.history)){
                var thisD = this;
                var historyContainer = d3.select(thisD).selectAll(".historyItem")
                    .data(d.history)
                    .enter();
//                console.log("History container", historyContainer);
                var historySvg = historyContainer.append("svg");
                historySvg.attr("class","historyContainer")
                    .attr("w",historyWidth)
                    .attr("h",historyHeight)
                    .attr("x",function(hd,hi){
                      return (hi * historyContainerWidth) + historyPositionX;
                    })
                    .attr("y",historyPositionY);
                historySvg.append("circle")
                    .attr("class","historyIndicator")
                    .attr("cy",dotRadius + 1)
                    .attr("cx",dotRadius + 1)
                    .attr("r",dotRadius)
                    .attr("stroke","black")
                    .attr("fill",function(hd,hi){
                      var hc = calcIndicatorDotColor(hd,true);
                      return hc;
                    });
                historySvg.append("text")
                    .attr("class","historyText")
                    .text(function(hd) {
                        return calcIndicatorText(hd);
                    })
                    .attr("x",(historyContainerWidth / 2) - 2)
                    .attr("y",(historyContainerHeight / 2) + 2)
                    .attr("text-anchor", "middle")
                    .attr("fill",function(hd,hi){ return calcIndicatorTextColor(hd); });
              }
        });

        var orbitingContainer = indicators.append("svg")
            .attr("class","orbitingContainer")
            .attr("w",orbitingContainerWidth)
            .attr("h",orbitingContainerHeight)
            .attr("x",function(d,i){
                 if ("lastCheck" in d && "period" in d && "severity" in d){
                     var center = calcRingX(d, i);
                     var radius = calcRingRadius(d);
                     var theta = (2 * Math.PI * calcIndicatorProportion(d)) - (Math.PI / 2);
                     return center + radius * Math.cos(theta) - (orbitingContainerWidth / 2);
                 } else {
                     return orbitingContainerWidth;
                 }
             })
            .attr("y",function(d,i){
               if ("lastCheck" in d && "period" in d && "severity" in d){
                   var center = calcRingY(d, i);
                   var radius = calcRingRadius(d);
                   var theta = (2 * Math.PI * calcIndicatorProportion(d)) - (Math.PI / 2);
                   return center + radius * Math.sin(theta) - (orbitingContainerHeight / 2);
               } else {
                   return orbitingContainerHeight;
               }
            });
        orbitingContainer.append("circle")
            .attr("class","orbitingIndicator")
            .attr("r",dotRadius)
            .attr("cx",orbitingContainerWidth / 2)
            .attr("cy",orbitingContainerHeight / 2)
            .attr("stroke","grey")
            .attr("fill",function(d){
                return calcIndicatorDotColor(d);
            })
            .attr("stroke-width",1);
        orbitingContainer.append("text")
            .attr("class","orbitingText")
            .text(function(d) {
                return calcIndicatorText(d);
            })
            .attr("x",(orbitingContainerWidth / 2))
            .attr("y",(orbitingContainerHeight / 2) + indicatorTextOffsetY)
            .attr("text-anchor", "middle")
            .attr("fill",function(d,i){ return calcIndicatorTextColor(d); });
        });
};