stack-all:
	stack-nightly build
	@echo
	stack-lts build
	@echo
	stack --resolver lts-14 build
#	@echo
#	stack --resolver lts-13 build
#	@echo
#	stack --resolver lts-12 build
#	@echo
#	stack --resolver lts-11 build
#	@echo
#	stack --resolver lts-10 build
