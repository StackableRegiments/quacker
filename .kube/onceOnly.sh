#!/bin/sh
NS="$1"
if [ -z "$NS" ];
then
echo "no namespace provided - using 'quacker'"
ENV="quacker"
fi
export PATH=~/kube:~/.local/bin:$PATH
mkdir envs
mkdir envs/$NS
SERVICE_FILE="envs/$NS/service.yaml"
kubectl create namespace $NS
mkdir envs
mkdir envs/$NS
SECRETS="envs/$NS/quacker-secrets.yaml"
if [ -f "$SECRETS" ];
then
./publishSecrets.sh $NS
else
cp quacker-secrets-template.yaml $SECRETS
mkdir envs/$NS/appConf
cp ../config/application.conf envs/$NS/appConf/application.conf
echo "set up with default secrets.  Please review the file envs/$NS/secrets.yaml and update the database credentials and application config, and then run:"
echo "./republish.sh $NS"
fi
