FROM archlinux:latest

RUN pacman -Syu --noconfirm
RUN pacman -S --noconfirm sbcl libev gcc
RUN curl -O https://beta.quicklisp.org/quicklisp.lisp
RUN sbcl --load quicklisp.lisp \
    --eval '(quicklisp-quickstart:install)' \
    --eval '(setf ql-util::*do-not-prompt* t)' \
    --eval '(ql:add-to-init-file)' \
    --eval '(quit)'

COPY . /cl-sqs
WORKDIR /cl-sqs

# This is to avoid re-installing packages each run.
RUN sbcl --load cl-sqs.asd \
    --eval '(ql:quickload :cl-sqs)'

CMD sbcl --load cl-sqs.asd \
    --eval '(ql:quickload :cl-sqs)' \
    --eval '(cl-sqs:start)'
