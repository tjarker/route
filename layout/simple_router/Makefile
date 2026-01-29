
gen:
	cd ../.. && sbt "runMain routing.simple.SimpleNoc"

harden:
	@rm -rf runs
	@rm -rf views
	librelane --ef-save-views-to views --run-tag simple_router simple_router.yaml