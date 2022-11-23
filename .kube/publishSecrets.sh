#!/bin/sh
NS="$1"
if [ -z "$NS" ];
then
echo "no namespace provided - using 'quacker'"
NS="quacker"
fi
kubectl delete secrets/app-conf-dir --namespace $NS
kubectl create secret generic app-conf-dir --from-file envs/$NS/appConf/ --namespace $NS
kubectl delete secrets/quacker-conf-dir --namespace $NS
kubectl create secret generic quacker-conf-dir --from-file envs/$NS/quackerConf/ --namespace $NS
kubectl delete secrets/quacker-secrets --namespace $NS
kubectl apply -f envs/$NS/quacker-secrets.yaml --namespace $NS
