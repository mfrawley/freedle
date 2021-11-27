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
	cd fable.test/ && dotnet test --no-restore --verbosity normal

release: clean restore build
	./release.sh
