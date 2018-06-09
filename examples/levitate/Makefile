.PHONY: test clean-artifacts

.build/release/levitate: Package.swift Source/levitate/main.swift
	swift build -c release

all: test

test: .build/release/levitate
	.build/release/levitate

clean-artifacts:
	-rm -rf .build

clean: clean-artifacts

swiftlint:
	swiftlint

lint: swiftlint
