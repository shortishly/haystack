# Haystack

Haystack is a HTTP load balancer integrated with
[docker](https://www.docker.com). As containers start or stop they
are added or removed from the load balancer.

## Quick Start

Haystack uses [SRV](https://en.wikipedia.org/wiki/SRV_record) records
to register docker containers. Haystack connects to Docker using the
`DOCKER_HOST` and `DOCKER_CERT_PATH` environment variables registering
any existing containers. Haystack continues to regsister (and
unregister) further containers as they stop and start.


Lets try this out, by starting up Haystack in docker:

```shell
docker run -e DOCKER_HOST=${DOCKER_HOST} \
           -e DOCKER_KEY="$(cat ${DOCKER_CERT_PATH}/key.pem)" \
           -e DOCKER_CERT="$(cat ${DOCKER_CERT_PATH}/cert.pem)" \
           --name=haystack \
           --publish=53:53 \
           --detach \
           shortishly/haystack:develop
```

As an example, create a pool of [nginx](https://www.nginx.com) servers:

```shell
docker run --detached --publish-all nginx
docker run --detached --publish-all nginx
```

Start a [busybox](https://www.busybox.net) terminal session with
Haystack providing the DNS:

```shell
docker run --dns=$(docker inspect --format='{{.NetworkSettings.IPAddress}}' haystack) \
           --tty \
           --interactive \
           --rm busybox \
           /bin/sh
```

Haystack will have automatically registered the nginx servers that we
created earlier. We can confirm this in our busybox session:

```shell
nslookup nginx.services.haystack
```

The DNS service should respond with an IP address for
`nginx.services.haystack`. Any docker container that exposes a HTTP
endpoint, will be automatically load balanced by Haystack.

We can test this by making a http request to `nginx.services.haystack`
and seeing which container responds:

```shell
wget http://nginx.services.haystack
```

> Connecting to nginx.services.haystack (172.17.0.3:80)<br />
> Connecting to da393gf.dockers.haystack:32841 (192.168.99.100:32841)

The request to `nginx.services.haystack` is initially handled by the
Haystack load balancer (in this case on on IP address 172.17.0.3). The
load balancer will randomly select a container that is providing the
`nginx` service - in this case the HTTP request is handled by the
container running on port 32841 on our docker host.

The load balancer will distribute load randomly over the
containers. Lets try making some more requests:

```shell
wget http://nginx.services.haystack
```

> Connecting to nginx.services.haystack (172.17.0.3:80)<br />
> Connecting to da393gf.dockers.haystack:32829 (192.168.99.100:32829)

This time the Haystack load balancer has randomly chosen the nginx
container that is running on port 32829. As you add or remove more
nginx containers from your docker host Haystack will automatically
update and distribute load accordingly. You can verify this by adding
some more nginx containers, and stopping some existing ones using the
appropriate commands in docker.

Any HTTP service can be automatically load balanced by Haystack. Lets
try some [Apache HTTP](https://hub.docker.com/_/httpd/) containers:

```shell
docker run --detached --publish-all httpd
```

Back in the busybox terminal:

```shell
wget http://httpd.services.haystack
```

Some services use the alt-HTTP port (8080). Lets try running a
[Jenkins](https://hub.docker.com/_/jenkins/) service:

```shell
docker run --detached --publish-all jenkins
```

Back again in the busybox terminal:

```shell
wget http://jenkins.services.haystack:8080/
```


## Building

Haystack uses [erlang.mk](https://github.com/ninenines/erlang.mk). To build run:

```
make
```

[![Build Status](https://travis-ci.org/shortishly/haystack.svg)](https://travis-ci.org/shortishly/haystack)

## Running

To run the release:

```
make run
```
