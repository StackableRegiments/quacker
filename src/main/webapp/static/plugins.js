var addPlugins = _.once(function(){
  var uiPluginContainer = $("#uiPluginContainer");
  if ("paused" in window) {
    var pauseButton = $("<span/>",{id:"pauseButton",class:"icon pluginButton"}).appendTo(uiPluginContainer);
    var getPauseIcon = function(paused) {
        return paused ? '\uf04b' : '\uf04c';
    };
    function pauseButtonClicked() {
      paused = !paused;
      pauseButton.text(getPauseIcon(paused));
    };
    pauseButton.on("click",pauseButtonClicked).text(getPauseIcon(paused));
  }

  var ajaxHandler = function(url,description) {
    return function(){
      $.ajax({
        url:url,
        method:"GET",
        contentType:"text/plain",
        success:function(data){
          console.log(description+"succeeded:",data);
        },
        error:function(error){
          console.log(description+"failed:",error);
          alert(description+"failed:"+JSON.stringify(error));
        }
      });
    };
  };

  if(isDevMode) {
    $("<span/>",{id:"reloadXmlButton",class:"icon pluginButton",text:'\uf021'}).on("click",ajaxHandler("/reloadXml","Reload Xml")).appendTo(uiPluginContainer);
    $("<span/>",{id:"breakSomethingButton",class:"icon pluginButton",text:'\uf0e7'}).on("click",ajaxHandler("/breakSomething","Break Something")).appendTo(uiPluginContainer);
  }
});