FROM hseeberger/scala-sbt:latest

COPY . /benchmark

RUN cd /benchmark && sbt run
