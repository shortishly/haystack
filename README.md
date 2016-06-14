# Haystack

Haystack is a DNS based service discovery system and automatic load
balancer. It manages service discovery by monitoring the container
lifecycle through the docker API. It introspects container metadata
exposing available services within its own DNS. It also includes a
HTTP load balancer - as services start or stop they are maintained in
a service group that is automatically load balanced for the
clients. It is also integrated with Swarm and overlay based networks.

## Quick Start

Haystack connects to Docker using the `DOCKER_HOST` and
`DOCKER_CERT_PATH` environment variables registering any existing
containers. Haystack continues to regsister (and unregister) further
containers as they stop and start.

Lets try this out, by starting up Haystack in docker:

If you are using TLS (recommended):

```shell
docker run -e DOCKER_HOST=${DOCKER_HOST} \
           -e DOCKER_KEY="$(cat ${DOCKER_CERT_PATH}/key.pem)" \
           -e DOCKER_CERT="$(cat ${DOCKER_CERT_PATH}/cert.pem)" \
           --name=haystack \
           --publish=53:53/udp \
           --publish=80:80 \
           --publish=8080:8080 \
           --publish=22022:22 \
           --detach \
           shortishly/haystack
```

Otherwise without TLS:

```shell
docker run -e DOCKER_HOST=${DOCKER_HOST} \
           --name=haystack \
           --publish=53:53/udp \
           --publish=80:80 \
           --publish=8080:8080 \
           --publish=22022:22 \
           --detach \
           shortishly/haystack
```

You may need to ensure that your firewall is allowing access to this
port. Consult your local documentation, on Fedora you can check
whether access is enabled via:

```shell
sudo firewall-cmd --list-ports
```

When not using TLS it should output something like:

```shell
2375/tcp
```

Where 2375 is the TCP port used by Docker. You can quickly open access
to your Docker daemon via:

```shell
sudo firewall-cmd --add-port=2375/tcp
```

As an example, lets create a pool of [nginx](https://www.nginx.com)
servers - each one is serving HTTP content from its own directory so
that we can see Haystack load balancing over the containers.

```shell
mkdir a b c d

echo a>a/index.html
echo b>b/index.html
echo c>c/index.html
echo d>d/index.html

docker run -v $(pwd)/a:/usr/share/nginx/html:ro -d nginx
docker run -v $(pwd)/b:/usr/share/nginx/html:ro -d nginx
docker run -v $(pwd)/c:/usr/share/nginx/html:ro -d nginx
docker run -v $(pwd)/d:/usr/share/nginx/html:ro -d nginx
```

Sometimes it is necessary to group a bunch of services - lets create a
`web` group too:

```shell
docker run --detach --name web-001 nginx
docker run --detach --name web-002 nginx
```

Haystack will automatically load balance services that are part of a
group. A group is identified by its name, followed by a dash and then
a number.

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
# wget -q -O /dev/stdout http://nginx.services.haystack
c
# wget -q -O /dev/stdout http://nginx.services.haystack
a
# wget -q -O /dev/stdout http://nginx.services.haystack
b
# wget -q -O /dev/stdout http://nginx.services.haystack
c
# wget -q -O /dev/stdout http://nginx.services.haystack
b
# wget -q -O /dev/stdout http://nginx.services.haystack
d
```

You can stop/kill one or more of the nginx containers above and
Haystack will automatically load balance over the remaining
containers. Haystack will remove `nginx.services.haystack` from its
DNS when the last nginx container is gone.

We can also load balance over the `web` group that we created earlier
by using:

```shell
wget -q -O /dev/stdout http://web.nginx.services.haystack
```

You'll notice that your HTTP requests are being handled by a different
set of nginx servers compared to those previously above.

Any HTTP service can be automatically load balanced by Haystack. Lets
try some [Apache HTTP](https://hub.docker.com/_/httpd/) containers:

```shell
docker run --detached httpd
```

Back in the busybox terminal:

```shell
wget http://httpd.services.haystack
```

Some services use the alt-HTTP port (8080). Lets try running a
[Jenkins](https://hub.docker.com/_/jenkins/) service:

```shell
docker run --detached jenkins
```

Back again in the busybox terminal:

```shell
wget http://jenkins.services.haystack:8080/
```

# Maintenance

Haystack runs a SSHD for maintenance or debugging the system. To
access Haystack you should add your public key to the authorised keys
that Haystack accepts.

```shell
ssh -p 22022 \
    $(docker inspect --format='{{.NetworkSettings.IPAddress}}' haystack)
```

If you are using `docker-machine`:

```shell
ssh -p 22022 $(docker-machine ip dev)
```

Where `dev` is the name of your `docker-machine` environment that you
are using.


Tracing for Haystack can be enabled via:
```shell
haystack:trace(true).
```

Using `false` will disable tracing.

Use `exit()` to exit from the Haystack shell.


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
