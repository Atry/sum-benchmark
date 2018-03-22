FROM hseeberger/scala-sbt:latest

COPY *.* .

RUN sbt run
