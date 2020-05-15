FROM ubuntu:bionic

COPY rabbitmq.config /etc/rabbitmq/rabbitmq.config
COPY enabled_plugins /etc/rabbitmq/enabled_plugins
COPY definitions.json /etc/rabbitmq/definitions.json

RUN apt update; apt install -y rabbitmq-server net-tools curl telnet vim less

EXPOSE 5672 15672

CMD ["/bin/bash", "-c", "set -e && rabbitmq-server"]
