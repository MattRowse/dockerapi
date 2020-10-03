setwd(getwd())
library(plumber)
r <- plumb("rest_controller.R")
r$run(port=80, host="0.0.0.0")

#below are terminal commands
#pull in base image
#docker pull rocker/r-ver:3.5.0

#run base image
#docker run -ti rocker/r-ver:3.5.0 

#build image and call it plumber-demo
#docker build -t plumber-demo .

#run on port 80
#docker run --rm -p 80:80 plumber-demo

#view the api is working at this link
#http://127.0.0.1/predict_petal_length?petal_width=1

#tag the image name and dockerhub address
#docker tag plumber-demo mattwanz/plumber-demo

# push the image to dockerhub
#docker push mattwanz/plumber-demo

# droplet instructions ssh in
# sudo apt update
# sudo apt install apt-transport-https ca-certificates curl software-properties-common
# curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo apt-key add -
# sudo add-apt-repository "deb [arch=amd64] https://download.docker.com/linux/ubuntu bionic stable"
# sudo apt update
# apt-cache policy docker-ce
# sudo apt install docker-ce

# check if it's working
# sudo systemctl status docker

# pull in the image from dockerhub
# docker pull mattwanz/plumber-demo

# run the service
# docker run --rm -p 80:80 mattwanz/plumber-demo

