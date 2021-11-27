.PHONY: build

clean:
	dotnet clean

restore:
	dotnet restore

build: 
	dotnet build --configuration Release --no-restore

build_release: clean restore
	dotnet pack -c Release

test:
	dotnet test fable.test/fable.test.fsproj --no-restore

release: clean restore build
	./release.sh
