# Haystack

Haystack is a minimal
[DNS](https://en.wikipedia.org/wiki/Domain_Name_System) server written
in [Erlang](http://erlang.org).

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

## Docker

Haystack is available as an automated build from
[hub.docker.com](https://hub.docker.com/r/shortishly/haystack/). Simply
use the following commands to run your own Haystack instance:

```
docker run --name haystack -p 3535:3535/udp -p 8080:8080 -d shortishly/haystack:develop
```

You can use port 8080 to POST zone records to Haystack, and port 3535
(UDP) to issue DNS queries or updates.

To upload a zone file to Haystack:

```
curl -i -H "Content-Type: text/dns" --data-binary @test/haystack_zone_SUITE_data/sample026.zone $(docker-machine ip default):8080/zones
```

And to query or update records:

```
dig @$(docker-machine ip default) -p 3535 example.test. soa
```


## Quick start

We will use
[ddns-confgen](http://ftp.isc.org/isc/bind9/9.9.0rc1/bind-9.9.0rc1/bin/confgen/ddns-confgen.html)
and [nsupdate](https://en.wikipedia.org/wiki/Nsupdate) to update some
records in the `example.test` DNS domain.

We need to make Haystack an authority for the `example.test` domain by
adding a
[SOA](https://en.wikipedia.org/wiki/List_of_DNS_record_types#SOA)
record for the `example.test` domain.

```
curl -i -H "Content-Type: text/dns" --data-binary @test/haystack_zone_SUITE_data/sample026.zone localhost:8080/zones
```

We can verify that this SOA is now present in Haystack with a simple dig query:

```shell
dig @localhost -p 3535 example.test soa
```

We will create a new key that will be used to authorise the
updates on our registry:

```shell
ddns-confgen -q -a hmac-md5 -k key.example.test -s example.test>key.example.test
```

The above command creates a key that will be used by subsequent
updates to the registry. We will assume that the newly created key is
as follows:

> key "key.example.test" {<br />
>	algorithm hmac-md5;<br />
>	secret "BriKgwLS0+O8tRXI7au/fw==";<br />
>};<br />

A copy of this key is provided as part of the test suite for Haystack,
and is used in the following command adding this shared secret to the
registry:

```shell
curl -i -H "Content-type: text/plain" --data-binary @test/haystack_secret_SUITE_data/sample001.key localhost:8080/secrets
```

Using the same secret with `nsupdate` to update the registry:

```shell
nsupdate -l -p 3535 -k test/haystack_secret_SUITE_data/sample001.key
```
> update delete haystack.example.test A<br />
> update add haystack.example.test 86400 A 10.1.2.3<br />
> update add haystack.example.test 86400 A 10.1.2.4<br />
> update add haystack.example.test 86400 A 10.1.2.5<br />
> send<br />
> quit<br />

Verify the updates with:

```shell
dig @localhost -p 3535 haystack.example.test a
```
