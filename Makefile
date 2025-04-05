out/debug:
	mkdir -p out/debug

out/release:
	mkdir -p out/release

clean:
	rm -rf out
	scala-cli clean .

SUFFIX = $(shell bash -c "cat .build.scala | scala-cli run _ -M coursierName")
LTO_TYPE = $(shell bash -c "cat .build.scala | scala-cli run _ -M ltoFlag")
BINARY_NAME = "trigs"

debug-bin: out/debug
	scala-cli package . -f -o out/debug/$(BINARY_NAME)

watch:
	scala-cli package -w . -f -o out/debug/$(BINARY_NAME)

out/flags/lto:
	mkdir -p out/flags
	cat .build.scala | scala-cli run _ -M ltoFlag > out/flags/lto

out/flags/platform:
	mkdir -p out/flags
	cat .build.scala | scala-cli run _ -M coursierName > out/flags/platform

bin: out/release out/flags/lto
	scala-cli package . -f -o out/release/$(BINARY_NAME) --native-mode release-fast $(LTO_TYPE)

platform-bin: out/release out/flags/platform out/flags/lto
	scala-cli package . -f -o out/release/$(BINARY_NAME)-$$(cat out/flags/platform) --native-mode release-fast $$(cat out/flags/lto)

check-docs:
	scala-cli compile README.md lib bin *.scala

test-lib:
	scala-cli test lib project.scala
	scala-cli test lib project.scala --native

publish-lib-local:
	scala-cli publish local lib project.scala --signer none --workspace .
	scala-cli publish local lib project.scala --native --signer none --workspace .

publish-lib-snapshot:
	scala-cli config publish.credentials oss.sonatype.org env:SONATYPE_USERNAME env:SONATYPE_PASSWORD
	scala-cli publish lib project.scala --signer none --workspace .
	scala-cli publish lib project.scala --native --signer none --workspace .

publish-lib:
	scala-cli config publish.credentials oss.sonatype.org env:SONATYPE_USERNAME env:SONATYPE_PASSWORD
	./.github/workflows/import-gpg.sh
	scala-cli publish lib project.scala --signer gpg --gpg-key 9D8EF0F74E5D78A3 --workspace .
	scala-cli publish lib project.scala --native --signer gpg --gpg-key 9D8EF0F74E5D78A3 --workspace .

code-check:
	scala-cli fmt . --check

pre-ci:
	scala-cli fmt .
