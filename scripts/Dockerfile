FROM python:3.6.4-slim-jessie
RUN apt update && apt install -y qt5-default libqt5webkit5-dev xvfb build-essential

ADD requirements.txt /root/
RUN pip install -r /root/requirements.txt

RUN adduser --disabled-password --gecos '' kasti

USER kasti
WORKDIR /home/kasti
ADD . /home/kasti
