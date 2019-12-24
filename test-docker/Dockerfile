FROM eshamster/cl-base:2.4.1

ADD ./test.sh /root
RUN ros install ccl-bin && \
    ros install rove

CMD ["/root/test.sh"]
