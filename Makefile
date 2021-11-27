.PHONY: build

RELEASE=Debug

clean:
	dotnet clean

restore:
	dotnet restore

build: 
	dotnet build --configuration ${RELEASE} --no-restore

build_release: clean restore
	dotnet pack -c ${RELEASE}

test:
	dotnet test fable.test/fable.test.fsproj --no-restore

release: clean restore build_release test
	./release.sh
