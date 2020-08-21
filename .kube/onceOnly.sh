#!/bin/sh
if [ -z "$1" ];
then
echo "please provide a namespace for this service."
exit 1
fi
export PATH=~/kube:~/.local/bin:$PATH
NS="$1"
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
cp ../appConf/application.conf envs/$NS/appConf/application.conf
echo "set up with default secrets.  Please review the file envs/$NS/secrets.yaml and update the database credentials and application config, and then run:"
echo "./republish.sh $NS"
fi
