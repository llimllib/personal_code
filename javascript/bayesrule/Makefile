.PHONY: all
all:
	s3cmd sync --acl-public . s3://llimllib/pretest.page/
	doctl compute cdn flush \
		$$(doctl compute cdn list --format ID | tail -n1) \
		--files pretest.page/*
