echo "please ensure that you've added the config to the quacker, to enable appropriate checks, by putting the xml files into the config directory"
docker stop $(docker ps -a -q)
docker rm $(docker ps -a -q)
sbt.sh compile
sbt.sh package
mkdir -p webapps
cp target/scala-2.11/*.war webapps/quacker.war
docker build -t quacker .
docker run -d -p 443:443 -it quacker
