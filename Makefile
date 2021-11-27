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
	dotnet nuget push bin/Debug/Freedle.0.0.3-alpha.nupkg --api-key $NUGET_KEY --source https://api.nuget.org/v3/index.json
