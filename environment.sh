if [[ $# -ne 1 ]] ; then
    echo 'Usage: Provide a environment.  This must be one of the configured "Container clusters" in your project'
    echo 'For instance:'
    echo './environment.sh qat'
    exit 1
fi
#Set the kube region or it will point to localhost and complain
gcloud config set compute/zone asia-east1-a
#Connect to the right cluster.  This contextualises further operations
gcloud container clusters get-credentials ${1}
