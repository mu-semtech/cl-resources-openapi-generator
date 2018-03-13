FROM semtech/mu-cl-resources:1.16.0

COPY . /app/
ADD ./startup.lisp /usr/src/startup.lisp

CMD sh /load-config.sh; sh mkdir /config/output; sbcl --load /usr/src/startup.lisp
