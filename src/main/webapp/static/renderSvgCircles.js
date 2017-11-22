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