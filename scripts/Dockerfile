FROM python:3.6.4-slim-jessie
RUN apt update && apt install -y libpq-dev libpq5 qt5-default libqt5webkit5-dev xvfb
RUN pip install lxml dryscrape PyRSS2Gen boto3 psycopg2

RUN adduser --disabled-password --gecos '' kasti

USER kasti
WORKDIR /home/kasti
ADD . /home/kasti
