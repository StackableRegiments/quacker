#!/bin/bash
if [ -z "$1" ];
then
echo "please provide a namespace for this console"
exit 1
fi
if [ -z "$2" ];
then
echo "please provide a podname for this console"
exit 1
fi

NS=$1
POD=$2
kubectl -n $NS logs --follow $POD 
