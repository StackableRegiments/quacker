#!/bin/sh
ENV_SET=$1

if [ -z "$ENV_SET" ];
then
echo "please provide an environment set for this republish.  it should be either upper or lower"
exit 1
fi

case $ENV_SET in
	lower)
		associate.sh arn:aws:eks:us-east-1:541765997109:cluster/ucroo-eks-lower
		associate.sh
		;;
	upper)
		associate.sh arn:aws:eks:us-east-1:541765997109:cluster/ucroo-eks
		associate.sh
		;;
	*)
		echo "unknown set.  please provide either upper or lower"
		exit 1
		;;
esac

$(aws ecr get-login --no-include-email)

for ENV in $(cat $ENV_SET)
do
	./republish.sh $ENV
done

./watchkube.sh
