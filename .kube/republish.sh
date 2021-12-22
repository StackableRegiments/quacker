#!/bin/sh
NS="$1"
if [ -z "$NS" ];
then
echo "no namespace provided - using 'quacker'"
NS="quacker"
fi

function exit_if_failed() {
	CMD_RESULT="$?"
	if [ $CMD_RESULT -ne "0" ];
	then	
		exit $CMD_RESULT;
	fi
}
REPO_HOST="541765997109.dkr.ecr.us-east-1.amazonaws.com"
REPO="$REPO_HOST/ucroo/quacker"

function login_to_ecr() {
	aws ecr get-login-password --region us-east-1 | docker login --username AWS --password-stdin $REPO_HOST
}

export PATH=~/kube:~/.local/bin:$PATH
GIT_REV="$(git rev-parse HEAD)"
docker login
IMAGE_META="$( aws ecr describe-images --repository-name=ucroo/quacker --image-ids=imageTag=$GIT_REV )"
## docker pull $REPO:$GIT_REV
if [ $? -eq 0 ];
then
	echo "image already exists";
else 
	echo "image not yet created - creating";
	cd ..
	./sbt.sh clean compile package
	exit_if_failed
	cd .kube
	cp ../target/scala-2.12/*.war ./root.war
	cd ..
	./sbt.sh clean
	exit_if_failed
	cd .kube
	login_to_ecr
	exit_if_failed
	# pull the latest version of the jetty image
#	docker pull jetty/jetty:latest
	docker image build --file Dockerfile --label quacker --tag 541765997109.dkr.ecr.us-east-1.amazonaws.com/ucroo/quacker:$GIT_REV --tag 541765997109.dkr.ecr.us-east-1.amazonaws.com/ucroo/quacker:latest .
	exit_if_failed
	rm ./root.war
	docker push 541765997109.dkr.ecr.us-east-1.amazonaws.com/ucroo/quacker:$GIT_REV
	exit_if_failed
	docker push 541765997109.dkr.ecr.us-east-1.amazonaws.com/ucroo/quacker:latest
	exit_if_failed
	docker rmi 541765997109.dkr.ecr.us-east-1.amazonaws.com/ucroo/quacker:latest
fi	
docker rmi 541765997109.dkr.ecr.us-east-1.amazonaws.com/ucroo/quacker:$GIT_REV
mkdir envs
mkdir envs/$NS
SECRETS="envs/$NS/quacker-secrets.yaml"
if [ -f "$SECRETS" ];
then
	./publishSecrets.sh $NS
	exit_if_failed
else
	cp quacker-secrets-template.yaml $SECRETS
	mkdir envs/$NS/appConf
	cp ../appConf/* envs/$NS/appConf/
	echo "no secrets available.  Please review the file envs/$NS/secrets.yaml and update the details in envs/$NS/appConf/, and then re-run this command."
	exit 1;
fi
DEPLOYMENT_FILE="envs/$NS/deployment.yaml"
cp deployment-template.yaml $DEPLOYMENT_FILE
exit_if_failed
sed -i "s/git-rev:.*\$/git-rev: $GIT_REV/g" $DEPLOYMENT_FILE
exit_if_failed

MEMORY_REQUEST_FILE="envs/$NS/deployment-settings/memory_requests"
if [ -f $MEMORY_REQUEST_FILE ];
then
  sed -i "s/memory:.*\$/memory: $(cat $MEMORY_REQUEST_FILE)/g" $DEPLOYMENT_FILE
  exit_if_failed
fi

CPU_LIMIT_FILE="envs/$NS/deployment-settings/cpu_limit"
if [ -f $CPU_LIMIT_FILE ];
then
  sed -i "s/cpu:.*\$/cpu: $(cat $CPU_LIMIT_FILE)/g" $DEPLOYMENT_FILE
  exit_if_failed
fi

MAX_MEM_FILE="envs/$NS/deployment-settings/JAVA_MAX_MEM"
MAX_MEM=$(cat ./default-max-mem)
if [ -f $MAX_MEM_FILE ];
then
  MAX_MEM=$(cat $MAX_MEM_FILE)
fi

sed -i "s/<<MAX_MEM>>/$MAX_MEM/g" $DEPLOYMENT_FILE

JAVA_OVERHEAD_FILE="envs/$NS/deployment-settings/JAVA_OVERHEAD_MEM"
JAVA_OVERHEAD_MEM=$(cat ./default-overhead-mem)
if [ -f $JAVA_OVERHEAD_FILE ];
then
        JAVA_OVERHEAD_MEM=$(cat $JAVA_OVERHEAD_FILE)
fi

JAVA_MAX_MEM=$( expr $MAX_MEM - $JAVA_OVERHEAD_MEM  )

sed -i "s/<<JAVA_OVERHEAD_MEM>>/$JAVA_OVERHEAD_MEM/g" $DEPLOYMENT_FILE
exit_if_failed

sed -i "s/<<JAVA_MAX_MEM>>/$JAVA_MAX_MEM/g" $DEPLOYMENT_FILE
exit_if_failed

echo "max mem $MAX_MEM"
echo "java overhead $JAVA_OVERHEAD_MEM"
echo "java heap $JAVA_MAX_MEM"

JAVA_OPTIONS_FILE="envs/$NS/deployment-settings/JAVA_OPTIONS"
if [ -f $JAVA_OPTIONS_FILE ];
then
  #sed -i "/name: \"JAVA_OPTIONS\"/{$!{N;s/value:.*$/value: \"$(cat $JAVA_OPTIONS_FILE)\"/;ty;P;D;:y}}" $DEPLOYMENT_FILE
  sed -i "s/<<ADDITIONAL_JAVA_ARGS>>/$(cat $JAVA_OPTIONS_FILE)/g" $DEPLOYMENT_FILE
  exit_if_failed
else
  sed -i "s/<<ADDITIONAL_JAVA_ARGS>>/$(cat ./default-java-options)/g" $DEPLOYMENT_FILE
  exit_if_failed
fi

sed -i "s/image: 541765997109.dkr.ecr.us-east-1.amazonaws.com\/ucroo\/quacker:.*\$/image: 541765997109.dkr.ecr.us-east-1.amazonaws.com\/ucroo\/quacker:$GIT_REV/g" $DEPLOYMENT_FILE
exit_if_failed

CLUSTER_SIZE_FILE="envs/$NS/deployment-settings/cluster-size"
if [ -f $CLUSTER_SIZE_FILE ];
then
	sed -i "s/replicas:.*\$/replicas: $(cat $CLUSTER_SIZE_FILE)/g" $DEPLOYMENT_FILE
	exit_if_failed
fi

PROBES_FILE="envs/$NS/deployment-settings/probes.yaml"
if [ -f $PROBES_FILE ];
then
	cat $PROBES_FILE >> $DEPLOYMENT_FILE
	exit_if_failed
else
	cat ./probes.yaml >> $DEPLOYMENT_FILE
	exit_if_failed
fi

sed -i "s/NS_DEFAULT\$/$NS/g" $DEPLOYMENT_FILE
exit_if_failed

CERT_ARN_FILE="envs/$NS/deployment-settings/certificate-arn"
SERVICE_FILE="envs/$NS/service.yaml"

if [ -f "envs/$NS/deployment-settings/single-server" ];
then
	echo single-server
	kubectl -n $NS delete service quacker-app-service
	kubectl -n $NS delete ingress haproxy-quacker-lb
	cp service-template.yaml $SERVICE_FILE
	exit_if_failed
	if [ -f $CERT_ARN_FILE ];
	then
		CERT=$(cat $CERT_ARN_FILE)
		sed -i "s=aws-load-balancer-ssl-cert:.*\$=aws-load-balancer-ssl-cert: \"$CERT\"=g" $SERVICE_FILE
		exit_if_failed
	fi
	kubectl -n $NS apply -f $SERVICE_FILE
	exit_if_failed
else	
	echo load-balanced by alb
	INGRESS_FILE="envs/$NS/alb-ingress.yaml"
	cp alb-ingress.yaml $INGRESS_FILE
	exit_if_failed
	sed -i "s/<<namespace>>/$NS/g" $INGRESS_FILE
	exit_if_failed
	sed -i "s=<<certificate-arn>>=\"$(cat $CERT_ARN_FILE)\"=g" $INGRESS_FILE
	exit_if_failed
	kubectl -n $NS apply -f $INGRESS_FILE
	exit_if_failed
fi

kubectl apply -f $DEPLOYMENT_FILE --namespace $NS
exit_if_failed
#watch kubectl describe deployment quacker --namespace $NS
