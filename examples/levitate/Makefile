.PHONY: test clean-artifacts clean-infer

.build/release/levitate: Package.swift Source/levitate/main.swift
	swift build -c release

all: test

test: .build/release/levitate
	.build/release/levitate

clean-artifacts:
	-rm -rf .build

clean-infer:
	-rm -rf infer-out

clean: clean-artifacts clean-infer

swiftlint:
	swiftlint

infer: clean
	infer -- make

lint: swiftlint infer
