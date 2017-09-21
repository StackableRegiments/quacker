var renderSvg = function(rootId){
    var svg = d3.select(rootId).append("svg");
    var data = _.toPairs(_.mapValues(_.groupBy(jsonStructure,function(item){ return item.service; }),function(v,k){
        return _.toPairs(_.groupBy(v,function(vi,vk){
            return vi.server;
        }));
    }));

    var constructIdentity = function(inString){
        return _.replace(inString," ","_");
    };
    // console.log("serviceData",data);
    var dGroup = {
        width:200,
        height:250
    };
    var services = svg
        .selectAll(".service")
        .data(data)
        .enter().append("g")
        .attr("class","service")
        .attr("id",function(d,i){
            return constructIdentity("service_"+d[0]);
        })
        .attr("height",dGroup.height)
        .attr("width",dGroup.width)
        .attr("transform",function(d,i){
            return sprintf("translate(%s,%s)",i * dGroup.width, 0);
        });
    var serviceLabels = services
        .append("text")
        .attr("class","serviceLabel")
        .attr("text-anchor","middle")
        .attr("x",dGroup.width/2)
        .attr("y",dGroup.height/2 + dGroup.height / 2)
        .text(function(d,i){
            //console.log("Service label:",d,i);
            return d[0];
        })
        .on("click",function(d,i){
            var selectorString = "#"+constructIdentity("service_"+d[0])+" .server";
            console.log("showing:",selectorString);
            $(selectorString).show();
        });
    var servers = services
        .selectAll(".server")
        .data(function(d){
            return d[1];
        })
        .enter().append("g")
        .attr("class","server")
        .attr("id",function(d,i){
            return constructIdentity("server_"+d[0]);
        });
    var serverLabels = servers
        .append("text")
        .attr("class","serverLabel")
        .attr("x",60)
        .attr("y",function(d,i){
            return dGroup.height/2 + (i * 30) + dGroup.height / 2 + 30;
        })
        .text(function(d){
            return d[0];//d.label;
        });
    var checks = servers
        .selectAll(".check")
        .data(function(d,i){
            return d[1];
        })
        .enter().append("g")
        .attr("class","check")
        .attr("id",function(d,i){
            return constructIdentity("check_"+d[0]);
        });
    var checkLabels = checks
        .append("text")
        .attr("class","checkLabel")
        .attr("x",90)
        .attr("y",function(d,i){
            return dGroup.height + i * 30;
        })
        .text(function(d,i){
            return d.label;
        })
        .on("mouseover",function(d,i){
            console.log('mouseOverElem',this,d,i);
        });
    var rings = checks
        .append("g")
        .data([30,60,90])
        .append("circle")
        .attr("cx",dGroup.width/2)
        .attr("cy",dGroup.height/2)
        .attr("r",function(d){return d})
        .attr("stroke","black")
        .attr("fill","none")
        .attr("stroke-width",2);
};
