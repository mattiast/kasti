FROM python:3.7

ADD requirements.txt /root/
RUN pip install -r /root/requirements.txt

RUN adduser --disabled-password --gecos '' kasti

USER kasti
WORKDIR /home/kasti
ADD ./*.py /home/kasti/
