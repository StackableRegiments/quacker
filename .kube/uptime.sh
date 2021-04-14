#!/bin/bash
if [ -z "$1" ];
then
echo "please provide a namespace for this republish"
exit 1
fi

function exit_if_failed() {
	CMD_RESULT="$?"
	if [ $CMD_RESULT -ne "0" ];
	then	
		exit $CMD_RESULT;
	fi
}

NS="$1"
export PATH=~/kube:~/.local/bin:$PATH

DEPLOYMENT_FILE="envs/$NS/deployment.yaml"

CLUSTER_SIZE_FILE="envs/$NS/deployment-settings/cluster-size"
if [ -f $CLUSTER_SIZE_FILE ];
then
	sed -i "s/replicas: 0\$/replicas: $(cat $CLUSTER_SIZE_FILE)/g" $DEPLOYMENT_FILE
	exit_if_failed
else
	sed -i "s/replicas: 0\$/replicas: 1/g" $DEPLOYMENT_FILE
	exit_if_failed
fi

kubectl apply -f $DEPLOYMENT_FILE --namespace $NS
exit_if_failed
