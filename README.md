# Haystack

Haystack is an automatic HTTP load balancer integrated with
[docker](https://www.docker.com). As containers start or stop they
are added or removed from the load balancer.

## Quick Start

Haystack automatically registers and unregisters docker containers
using [SRV](https://en.wikipedia.org/wiki/SRV_record)
records. Haystack will automatically connect to Docker using the
`DOCKER_HOST` and `DOCKER_CERT_PATH` environment variables, and
register any existing containers, and will continue to regsister (or
unregister) further containers on that docker host.

Lets try this out, by starting up Haystack in docker:

```shell
docker run -e DOCKER_HOST=${DOCKER_HOST} \<br />
           -e DOCKER_KEY="$(cat ${DOCKER_CERT_PATH}/key.pem)" \<br />
           -e DOCKER_CERT="$(cat ${DOCKER_CERT_PATH}/cert.pem)" \<br />
           --name=haystack \<br/>
           -d shortishly/haystack:develop
```

As an example, lets create a pool of [nginx](https://www.nginx.com) servers:

```shell
docker run -d -P nginx
docker run -d -P nginx
```

Start a [busybox](https://www.busybox.net) terminal session with
Haystack providing the DNS:

```shell
docker run --dns=$(docker inspect --format='{{.NetworkSettings.IPAddress}}' haystack) \<br/>
           -t \<br/>
           -i \<br/>
           --rm busybox \<br/>
           /bin/sh
```

Haystack will have automatically registered the nginx servers that we
created earlier. We can confirm this by checking whether they are
available in our busybox session.

```shell
nslookup nginx.services.haystack
```

The DNS service should respond with an IP address for
`nginx.services.haystack`. Any docker container that exposes a HTTP
endpoint (on port 80 - as our nginx containers are), will be
automatically load balanced by Haystack.

We can test this by making a http request to `nginx.services.haystack`
and seeing which container responds:

```shell
wget http://nginx.services.haystack
```

> Connecting to nginx.services.haystack (172.17.0.3:80)
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

> Connecting to nginx.services.haystack (172.17.0.3:80)
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
docker run -d -P httpd
```

Back in the busybox terminal:

```shell
wget http://httpd.services.haystack
```

Some services use the alt-HTTP port (8080). Lets try running a
[Jenkins](https://hub.docker.com/_/jenkins/) service:

```shell
docker run -d -P jenkins
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
