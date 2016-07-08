FROM jetty:9

EXPOSE 8080

EXPOSE 443

COPY jettyConf/keystore /var/lib/jetty/etc/keystore

RUN chmod go-rwx /var/lib/jetty/etc/keystore

COPY webapps /var/lib/jetty/webapps

COPY jettyConf/* /var/lib/jetty/start.d/

COPY config /var/lib/jetty/config
