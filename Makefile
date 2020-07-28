stack-all:
	stack --resolver nightly build
	@echo
	stack --resolver lts build
	@echo
	stack --resolver lts-15 build
	@echo
	stack --resolver lts-14 --stack-yaml stack-lts14.yaml build
	@echo
	stack --resolver lts-13 --stack-yaml stack-lts13.yaml build
	@echo
	stack --resolver lts-12 --stack-yaml stack-lts12.yaml build
	@echo
	stack --resolver lts-11 --stack-yaml stack-lts12.yaml build
#	@echo
#	@echo bugzilla fails: setRequestCheckStatus
#	stack --resolver lts-10 --stack-yaml stack-lts10.yaml build
