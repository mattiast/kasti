FROM python:3.8-buster

ADD requirements.txt /root/
RUN pip install -r /root/requirements.txt

RUN adduser --disabled-password --gecos '' kasti

USER kasti
WORKDIR /home/kasti
ADD ./src/*.py /home/kasti/
CMD [ "sh", "-c", "exec uvicorn --host 0.0.0.0 --port $PORT main:app" ]
