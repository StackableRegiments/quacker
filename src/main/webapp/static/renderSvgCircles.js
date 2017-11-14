var renderCheckSvg = function(checkElem, allChecks) {
    var dGroup = {
        width: 250,
        height: 250
    };
    var ringRadius = 75;
    var orbitingContainerWidth = 25;
    var orbitingContainerHeight = 25;
    var dotRadius = 10;
    var indicatorTextOffsetY = 4;
    var historiesWidth = 27;
    var historiesHeight = 102;
    var historyContainerWidth = 25;
    var historyContainerHeight = 25;
    var historyContainerPositionX = 0;
    var historyContainerPositionY = 0;

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
    var calcIndicatorText = function(d,ignoreNextCheck) {
        // console.log("Calcing text",d,d.lastCheck,d.status,d.label);
        if("lastCheck" in d && "period" in d) {
            var nextCheck = (d.lastCheck + d.period);
            if (d.lastCheck == 0 || (!ignoreNextCheck && new Date().getTime() > nextCheck)) {
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
    var calcIndicatorTextColor = function(d,ignoreNextCheck){
        // console.log("Calcing color",d,d.lastCheck,d.status,d.label);
        if("lastCheck" in d && "period" in d) {
            var nextCheck = (d.lastCheck + d.period);
            if (d.lastCheck == 0 || (!ignoreNextCheck && new Date().getTime() > nextCheck)) {
                return "black";
            }
        }
        if("status" in d) {
            if (d.status == true) {
                return "lightgrey";
            } else {
                return "white";
            }
        }
        return "orange";
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
                var thisIndicatorD = this;
                var historyCount = _.size(d.history);
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
                    .data(d.history)
                    .enter();
                histories.each(function(hd,hi){
                    var currentHistoryIndex = historyCount - hi - 1;

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
                        .attr("stroke","black")
                        .attr("stroke-opacity",function(){
                            return calcIndicatorOpacity(d,currentHistoryIndex,historyCount);
                        })
                        .attr("fill",function(){
                            return calcIndicatorDotColor(hd,true);
                        })
                        .attr("fill-opacity",function(){
                            return calcIndicatorOpacity(d,currentHistoryIndex,historyCount);
                        });
                    historySvg.append("text")
                        .attr("class","historyText")
                        .text(function() {
                            return calcIndicatorText(hd,true);
                        })
                        .attr("x",(historyContainerWidth / 2) - 2)
                        .attr("y",(historyContainerHeight / 2) + 2)
                        .attr("text-anchor", "middle")
                        .attr("fill",function(){
                            return calcIndicatorTextColor(hd,true);
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