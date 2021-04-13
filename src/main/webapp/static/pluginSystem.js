if (!('size' in _ && 'keys' in _ && 'isFunction' in _ && 'forEach' in _)){
  throw 'this library requires the lodash library to be preloaded';
}

if (pluginSystem == undefined){
  var pluginSystem = {};
}
if (!(('listCommands' in pluginSystem) && ('registerCommand' in pluginSystem) && ('deregisterCommand' in pluginSystem) && ('subscribe' in pluginSystem) && ('unsubscribe' in pluginSystem) && ('suspendCommand' in pluginSystem) && ('resumeCommand' in pluginSystem) && ('fireCommand' in pluginSystem) && ('setDebug' in pluginSystem))){
  pluginSystem = (function(){
    var commands = {};
    var addCommand = function(name,before,after){
      commands[name] = {
        id:name,
        beforeFunc:before,
        afterFunc:after,
        suspended:false,
        interruptCount:0,
        subscriptions:{},
        queuedActions:[]
      };
    };
    var removeCommand = function(name){
      if (name in commands){
        delete commands[name];
      }
    };
    var addSubscription = function(name,funcId,func){
      if (name in commands){
        commands[name].subscriptions[funcId] = func;
      }
    };
    var removeSubscription = function(name,funcId){
      if (name in commands){
        var s = commands[name].subscriptions;
        delete s[funcId];
      }
    };
    var fireSubscription = function(command,subName,subscription,callerId){
			var dataPacket = _.drop(arguments,4);
      if (command != undefined && subscription != undefined){
        var start = Date.now();
        subscription.apply(this,dataPacket);
        var end = Date.now();
        if (command.id != 'commandTimed' && command.id != 'exceptionThrown'){
          activateCommand('commandTimed','pluginSystem.fireSubscription',{
            'start':start,
            'end':end,
            'duration':end - start,
            'command':command.id,
            'subscription':subName,
            'callerId':callerId,
            'dataPacket':dataPacket
          });
        }
      }
    }
    var runCommand = function(command,callerId){
			var dataPacket = _.drop(arguments,2);
      var throwEx = function(onAction,ex){
        return {
          'command':command,
          'callerId':callerId,
          'dataPacket':dataPacket,
          'action':onAction,
          'innerException':ex
        };
      };
      var exs = [];
			if ('beforeFunc' in command && command.beforeFunc != undefined && _.isFunction(command.beforeFunc)){
				try {
					command.beforeFunc();
				} catch(ex){
					exs.push(throwEx('beforeFunc',ex));
				}
			}
			if ('subscriptions' in command){
				_.forEach(_.keys(command.subscriptions),function(item){
					try {
						var subscription = command.subscriptions[item];
						var args = _.concat([command,item,subscription,callerId],dataPacket)
						fireSubscription.apply(this,args);
					} catch(ex){
						exs.push(throwEx(item,ex));
					}
				});
			}
			if ('afterFunc' in command && command.beforeFunc != undefined && _.isFunction(command.afterFunc)){
				try {
					command.afterFunc();
				} catch(ex){
					exs.push(throwEx('afterFunc',ex));
				}
			}
			_.forEach(exs,function(e){
				activateCommand('exceptionThrown','pluginSystem.runCommand',e);
			});
    };
    var activateCommand = function(name,callerId){
			var dataPacket = _.drop(arguments,2);
      if (name in commands){
        var c = commands[name];
        if (c.suspended){
          if (callerId != undefined && 'queuedActions' in c){
            c.queuedActions.push({'callerId':callerId,'data':dataPacket});
          }
        } else {
          while (_.size(c.queuedActions) > 0){
            var qi = c.queuedActions.pop();
            if ('data' in qi && 'callerId' in qi){
							runCommand.apply(this,_.concat([c,callerId],qi.data));
            }
          };
          if (callerId != undefined){
						runCommand.apply(this,_.concat([c,callerId],dataPacket));
          }
        }
      }
    }; 
    var pauseCommand = function(name){
      if (name in commands){
        var c = commands[name];
        c.interruptCount = c.interruptCount + 1;
        if (c.interruptCount > 0){
          commands[name].suspended = true;
        }
      }
    };
    var restartCommand = function(name){
      if (name in commands){
        var c = commands[name];
        c.interruptCount = c.interruptCount - 1;
        if (c.interruptCount < 1){
          c.suspended = false;
          activateCommand(name);
        }
      }
    };
    addCommand('commandTimed',function(){},function(){});
    addCommand('exceptionThrown',function(){},function(){});
    pauseCommand('exceptionThrown');
    return {
      listCommands:function(){
        return commands;
      },
      registerCommand:function(name,before,after){
        addCommand(name,before,after);
      },
      deregisterCommand:function(name){
        removeCommand(name);
      },
      subscribe:function(command,funcId,func){
        addSubscription(command,funcId,func);
      },
      unsubscribe:function(command,funcId){
        removeSubscription(command,funcId);
      },
      suspendCommand:function(command){ 
        pauseCommand(command);
      },
      resumeCommand:function(command){
        restartCommand(command);
      },
      fireCommand:function(command,caller,dataPacket){
				activateCommand.apply(this,arguments);
      },
      setDebug:function(bool,timingFunc,exFunc){
        var inTimingFunc = timingFunc;
        var inExFunc = exFunc;
        if (inTimingFunc == undefined){inTimingFunc = function(o){console.log('timed',o);};}
        if (inExFunc == undefined){inExFunc = function(o){console.log('exception',o);};}
        if (bool){
          addSubscription('commandTimed','pluginSystem.setDebug',inTimingFunc);
          addSubscription('exceptionThrown','pluginSystem.setDebug',inExFunc);
        } else {
          removeSubscription('commandTimed','pluginSystem.setDebug');
          removeSubscription('exceptionThrown','pluginSystem.setDebug');
        }
      }
    };
  })();
}
