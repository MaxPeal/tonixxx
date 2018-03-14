VERSION=0.0.1

.PHONY: lint install port uninstall clean clean-ports

govet:
	find . -path "*/vendor*" -prune -o -name "*.go" -type f -exec go tool vet -shadow {} \;

golint:
	find . -path '*/vendor/*' -prune -o -name '*.go' -type f -exec golint {} \;

gofmt:
	find . -path '*/vendor/*' -prune -o -name '*.go' -type f -exec gofmt -s -w {} \;

goimports:
	find . -path '*/vendor/*' -prune -o -name '*.go' -type f -exec goimports -w {} \;

errcheck:
	errcheck -blank

nakedret:
	nakedret -l 0 ./...

opennota-check:
	aligncheck
	structcheck
	varcheck

megacheck:
	megacheck

lint: govet golint gofmt goimports errcheck nakedret opennota-check megacheck

install:
	sh -c "cd cmd/tonixxx && go install"

bin:
	gox -output="bin/tonixxx-$(VERSION)/{{.OS}}/{{.Arch}}/{{.Dir}}" ./cmd/...

archive-ports: bin
	zipc -chdir bin "tonixxx-$(VERSION).zip" "tonixxx-$(VERSION)"

port: archive-ports

uninstall:
	-rm -rf "$$GOPATH/bin/tonixxx"

clean-ports:
	-rm -rf bin

clean: clean-ports
