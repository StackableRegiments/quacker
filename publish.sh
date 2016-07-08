#AUTHOR: chris.hagan@csc.gov.au

#This is a Scala Lift project.  It runs as a Docker container, running a standard WAR inside a vanilla Jetty application server
#The Docker container is mapped onto a Google Container Engine cluster using Kubernetes.
#To do so, we house the container image in the Google Container Registry for easy access, versioning and rollback.
#Performing these steps will require credential access to the Google Container Engine and to the Stackable Regiments code repository. 
#The steps are perfomed from within the Google cloud console.

echo 'This must be run from within the Google cloud console'
echo 'You must choose an environment first for predictable results, using the forms "environment.sh [prod|qat]"'

#Create the appropriate WAR

if [[ $# -ne 1 ]] ; then
    echo 'Usage: Provide a release tag.  It will apply to the uploaded image and to the deployment'
    echo 'For instance:'
    echo './publish.sh v1'
    exit 1
fi
RELEASE=${1}
PROJECT=quacker-1362
REPOSITORY=quacker
TAG=gcr.io/$PROJECT/$REPOSITORY:$RELEASE
VERSION=csc/$REPOSITORY:$RELEASE
echo $RELEASE $PROJECT $REPOSITORY $VERSION $TAG
#Pull the code if it's not there (but how would you be reading this?)
#git clone https://StackableRegiments@github.com/StackableRegiments/monitoring.git
#Update the code
git pull

#Create the release artifact
./sbt clean
./sbt compile
./sbt package
mkdir -p webapps
cp target/scala-2.11/*.war webapps/quacker.war

#Build the docker image.  This builds the project as well.
docker build -t  $VERSION .
#Tag the image
docker tag $VERSION $TAG
#Upload the image to the container repository
gcloud docker push $TAG
#Create a deployment

#The hostport ensures that only a single node can pick up each pod so that they cannot conflict on mapping addresses.  This ensures that session stickiness on Client IP will also not be ruined.
#kubectl run $REPOSITORY --image=gcr.io/$PROJECT/$REPOSITORY:$RELEASE --port=8080 --hostport=8080
#Create an external IP for it
#kubectl expose deployment $REPOSITORY --target-port=3000 --port=80 --type="LoadBalancer"

#gcloud compute firewall-rules create allow-130-211-0-0-22 --source-ranges 130.211.0.0/22  --allow tcp:30000-32767

#Edit an existing deployment
echo "Check that the tag went in (it will show up in the container registry), and go ahead and edit the deployment:"
echo "kubectl edit deployment $REPOSITORY"
echo "Find the version tag and update it"
echo "That's this line:"
echo "   - image: gcr.io/$PROJECT/$REPOSITORY:old_release_tag"
echo "Needs to be changed to"
echo "   - image: gcr.io/$PROJECT/$REPOSITORY:$RELEASE"


#We're done.  Now verify and work out how to address it
#The EXTERNAL_IP may take several minutes to become available and visible. If the EXTERNAL_IP is missing, wait a few minutes and try again.
#$ kubectl get services hello-node
#NAME         CLUSTER_IP    EXTERNAL_IP     PORT(S)    SELECTOR         AGE
#hello-node   10.3.246.12   23.251.159.72   80/TCP   run=hello-node   2m

#Publication of the DNS record is outside the control of this script
