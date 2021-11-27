.PHONY: build

clean:
	dotnet clean

restore:
	dotnet restore

build: 
	dotnet build 

build_release: clean restore
	dotnet pack -c Release

test:
	cd fable.test/ && dotnet test

release: clean restore build
	./release.sh
