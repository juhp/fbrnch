stack-all:
	stack-nightly build
	@echo
	stack-lts build
	@echo
	stack --resolver lts-14 build
	@echo
	stack --resolver lts-13 --stack-yaml stack-lts13.yaml build
	@echo
	stack --resolver lts-12 --stack-yaml stack-8.4.4.yaml build
	@echo
	stack --resolver lts-11 --stack-yaml stack-8.4.4.yaml build
