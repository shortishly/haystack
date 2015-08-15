# tshark
tshark -f "port 53" -w dns.pcap -c 20

# avahi


avahi-browse --domain=zeroconf.org _services._dns-sd._udp -t
avahi-browse --domain=zeroconf.org _ssh._tcp -r -t
avahi-browse --domain=zeroconf.org _printer._tcp -r -t
avahi-browse --domain=zeroconf.org _ftp._tcp -r -t
avahi-browse --domain=zeroconf.org _pdl-datastream._tcp -r -t

avahi-browse _services._dns-sd._udp -t
avahi-browse _adisk._tcp -r -t

dig _services._dns-sd._udp.statusbar.info. ptr
dig b._dns-sd._udp.statusbar.info. any
dig db._dns-sd._udp.statusbar.info. any
dig r._dns-sd._udp.statusbar.info. any
dig dr._dns-sd._udp.statusbar.info. any
dig lb._dns-sd._udp.statusbar.info. any

dig _http._tcp.mxtoolbox.com. any
dig _services._dns-sd._udp.apple.com ptr

dig _services._dns-sd._udp.zeroconf.org ptr
dig b._dns-sd._udp.zeroconf.org ptr
dig db._dns-sd._udp.zeroconf.org ptr
dig r._dns-sd._udp.zeroconf.org ptr
dig dr._dns-sd._udp.zeroconf.org ptr
dig lb._dns-sd._udp.zeroconf.org ptr


dig _ssh._tcp.zeroconf.org. any
dig Rose._ssh._tcp.zeroconf.org. any
dig _http._tcp.zeroconf.org. any

dig _services._dns-sd._udp.dns-sd.org any
dig _http._tcp.dns-sd.org. ptr
dig "\032*\032BBC,\032World\032news._http._tcp.dns-sd.org." any

dns-sd -E
