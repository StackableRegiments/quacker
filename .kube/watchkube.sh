#!/bin/sh
ENV="--all-namespaces=true"

WATCH_PODS="true"
WATCH_SERVICES=""
WATCH_DEPLOYMENTS=""
WATCH_NODES=""
WATCH_INGRESSES=""
EXCLUDE_KUBE="true"
EXCLUDE_AMAZON="true"

while test $# -gt 0
do
	case "$1" in
		"NODES")
			WATCH_NODES="true"
			;;
		"INGRESSES")
			WATCH_INGRESSES="true"
			;;
		"DEPLOYMENTS")
			WATCH_DEPLOYMENTS="true"
			;;
		"PODS")
			WATCH_PODS="true"
			;;
		"SERVICES")
			WATCH_SERVICES="true"
			;;
		"NO_SERVICES")
			WATCH_SERVICES=""
			;;
		"NO_PODS")
			WATCH_PODS=""
			;;
		"NO_DEPLOYMENTS")
			WATCH_DEPLOYMENTS=""
			;;
		"NO_NODES")
			WATCH_NODES=""
			;;
		"NO_INGRESSES")
			WATCH_INGRESSES=""
			;;
		"NO_AMAZON")
			EXCLUDE_AMAZON="true"
			;;
		"AMAZON")
			EXCLUDE_AMAZON=""
			;;
		"NO_KUBE")
			EXCLUDE_KUBE="true"
			;;
		"KUBE")
			EXCLUDE_KUBE=""
			;;
		*)
			ENV="-n $1"
			;;
	esac
	shift
done

SUFFIX=""

if [ -z "$EXCLUDE_AMAZON" ]
then
	ENV="$ENV"
else
	SUFFIX="$SUFFIX | grep -v amazon-cloudwatch"
fi

if [ -z "$EXCLUDE_KUBE" ]
then
	ENV="$ENV"
else
	SUFFIX="$SUFFIX | grep -v kube-system"
fi

if [ -z "$WATCH_PODS" ]
then
	ENV="$ENV"
else
WATCH_PODS="kubectl $ENV get pods -o wide $SUFFIX;"
fi

if [ -z "$WATCH_SERVICES" ]
then
	ENV="$ENV"
else
	WATCH_SERVICES="kubectl $ENV get services $SUFFIX;"
fi

if [ -z "$WATCH_DEPLOYMENTS" ]
then
	ENV="$ENV"
else
	WATCH_DEPLOYMENTS="kubectl $ENV get deployments $SUFFIX;"
fi

if [ -z "$WATCH_NODES" ]
then
	ENV="$ENV"
else
	WATCH_NODES="kubectl $ENV get nodes $SUFFIX;"
fi

if [ -z "$WATCH_INGRESSES" ]
then
	ENV="$ENV"
else
	WATCH_INGRESSES="kubectl $ENV get ingress $SUFFIX;"
fi


CMDSTRING="$WATCH_SERVICES $WATCH_INGRESSES $WATCH_NODES $WATCH_DEPLOYMENTS $WATCH_PODS"
watch "$CMDSTRING"

#watch "kubectl --all-namespaces=true get services; kubectl --all-namespaces=true get nodes; kubectl --all-namespaces=true get deployments; kubectl --all-namespaces=true get pods"
#else
#watch "kubectl -n $ENV get services; kubectl -n $ENV get nodes; kubectl -n $ENV get deployments; kubectl -n $ENV get pods"
#fi

