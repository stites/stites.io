---
date: 1900-01-01
---


What is Docker
Docker is a platform to run each application isolated and securely. Internally it
achieves it by using kernel containerization feature.

What is the advantage of Docker over hypervisors?
Docker is light weight and more efficient in terms of resource uses because it uses
the host underlying kernel rather than creating its own hypervisor. 

What is Docker Image
Docker Image is the source of the docker container. In other words, docker images are
used to create containers. It is possible create multiple isolated containers from a
single image.

What is Docker Container
Docker Container is the instantiation of docker image. In other words, it is the run
time instance of images. Images are set of files whereas containers is the one who
run the image inside isolated.

Difference between Docker Image and container?
Docker container is the runtime instance of docker image.
Docker Image doesnot have a state and its state never changes as it is just set of
files whereas docker container has its execution state.

