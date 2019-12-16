FROM eshamster/cl-base:2.4.1

ADD ./test.sh /root
RUN apk add --no-cache gcc libc-dev libuv-dev && \
    ros install ccl-bin && \
    ros install rove

CMD ["/root/test.sh"]
