FROM semtech/mu-cl-resources:1.13.0

COPY . /app/dependencies/resources-openapi-generator/
ADD ./startup.lisp /usr/src/startup.lisp

CMD sh /load-config.sh; sh mkdir /config/output; sbcl --load /usr/src/startup.lisp
